package raffle

import java.nio.charset.StandardCharsets
import java.util.Calendar

import dao.DonateReqDAO
import helpers.{Configs, Utils}
import io.circe.Json
import javax.inject.Inject
import models.DonateReq
import network.{Client, Explorer}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.{Address, ConstantsBuilder, ErgoId, ErgoToken, ErgoValue, InputBox}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import scorex.crypto.hash.Digest32
import special.collection.Coll

import scala.collection.mutable.Seq
import scala.collection.JavaConverters._

class DonateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract,
                               donateReqDAO: DonateReqDAO){


  def findProxyAddress(pk: String, raffleId: String, ticketCounts: Long): String = {
    client.getClient.execute(ctx => {

      val propByte = new ErgoTreeContract(Address.create(pk).getErgoAddress.script)
      val donateContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("tokenId", ErgoId.create(raffleId).getBytes)
          .item("pk", propByte.getErgoTree.bytes)
          .item("ticketCount", ticketCounts)
          .build(),
        raffleContract.donateScript)
      val feeEmissionAddress: ErgoAddress = Configs.addressEncoder.fromProposition(donateContract.getErgoTree).get

      val boxes = explorer.getUnspentTokenBoxes(raffleId, 0, 100).hcursor.downField("items").as[Array[Json]]
        .getOrElse(throw new Throwable("parse error"))

      // TODO: Write it better with map and filter
      var raffleBox : Json = null
      for (box <- boxes){
        try {
          val asset = box.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
          if (asset.hcursor.get[String]("tokenId").getOrElse("") == Configs.serviceTokenId) raffleBox = box
        }
      }

      val address = raffleBox.hcursor.get[String]("address").getOrElse("")
      println("Donate patment address created")
      // TODO: Edit it after changing raffle scripts
      // val ticketPrice =
      val currentTime = Calendar.getInstance().getTimeInMillis / 1000
      donateReqDAO.insert(ticketCounts, 10000000, 0, feeEmissionAddress.toString, address, raffleId,
        null, pk, "not-imp", currentTime + Configs.inf, currentTime + Configs.creationDelay)
      return feeEmissionAddress.toString
    })
  }

  def createDonateTx(req: DonateReq): Unit = {
    client.getClient.execute(ctx => {

      // TODO: Change this to chain the donation transactions
      val boxes = ctx.getUnspentBoxesFor(Address.create(req.raffleAddress))
      val raffleBox: InputBox = boxes.asScala.filter(box => box.getTokens.get(1).getId.toString == Configs.serviceTokenId)
        .filter(box => box.getTokens.get(0).getId.toString == req.raffleToken).head

      val paymentBoxList = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress))
      if (paymentBoxList.size() == 0) return
      var proxyBox: InputBox = null
      for (i <- 0 until paymentBoxList.size()) {
        if (paymentBoxList.get(i).getValue >= req.ticketPrice * req.ticketCount) {
          proxyBox = paymentBoxList.get(i)
        }
      }

      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val txB = ctx.newTxBuilder()

      val R4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[Long]
      val R5 = raffleBox.getRegisters.get(1).getValue.asInstanceOf[Long]
      val R6 = raffleBox.getRegisters.get(2).getValue.asInstanceOf[Long]
      val R7 = new String(raffleBox.getRegisters.get(3).getValue.asInstanceOf[Coll[Byte]].toArray,
        StandardCharsets.UTF_8)

      //println(R7)
      //println("OK")
      val ticketPrice = io.circe.jawn.parse(R7)
        .getOrElse(throw new Throwable("ticket parse error"))
        .hcursor.downField("ticketPrice").as[Long].getOrElse(throw new Throwable("ticketPrice not Found"))

      //println("OK")
      val deadlineHeight = io.circe.jawn.parse(R7)
        .getOrElse(throw new Throwable("deadLine parse error"))
        .hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadLine not Found"))

      //println(R7)

      //val tickets = (proxyBox.getValue - 2 * Configs.fee) / 1000000
      val newRaffleBox = txB.outBoxBuilder()
        .value(raffleBox.getValue + proxyBox.getValue - 2 * Configs.fee)
        .contract(new ErgoTreeContract(Address.create(req.raffleAddress).getErgoAddress.script))
        .tokens(new ErgoToken(raffleBox.getTokens.get(0).getId,  raffleBox.getTokens.get(0).getValue - req.ticketCount),
          new ErgoToken(raffleBox.getTokens.get(1).getId,  raffleBox.getTokens.get(1).getValue))
        .registers(ErgoValue.of((R4 + req.ticketCount).toLong) , ErgoValue.of(R5), ErgoValue.of(R6), ErgoValue.of(R7.getBytes("utf-8")))
        .build()

      val winnerContract = ctx.compileContract(
        ConstantsBuilder.create()
          .build(),
        raffleContract.winnerScript)

      val winnerErgoTree = winnerContract.getErgoTree
      val winnerScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(winnerErgoTree.bytes)

      // TODO: Change the place and remove update db
      val ticketContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("deadlineHeight", deadlineHeight)
          .item("winnerScriptHash", winnerScriptHash)
          .item("ticketPrice", 1000000L)
          //.item("projectPubKey", Configs.raffleProjectAddress.getPublicKey)
          .build(),
        raffleContract.ticketScript)

      val propByte = new ErgoTreeContract(Address.create(req.participantAddress).getErgoAddress.script)

      val scriptTokenRepoHash: Digest32 = scorex.crypto.hash.Blake2b256(raffleBox.getErgoTree.bytes)
      val TicketBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(ticketContract)
        .registers(ErgoValue.of(R4.toLong), ErgoValue.of(req.ticketCount), ErgoValue.of(scriptTokenRepoHash),
          ErgoValue.of(propByte.getErgoTree.bytes))
        .tokens(new ErgoToken(raffleBox.getTokens.get(0).getId, req.ticketCount))
        .build()

      val tx = txB.boxesToSpend(Seq(raffleBox, proxyBox).asJava)
        .fee(Configs.fee)
        .outputs(newRaffleBox, TicketBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTx = prover.sign(tx)
      donateReqDAO.updateSignedDonateTx(req.id, signedTx.toJson(false))
      donateReqDAO.updateTicketAddress(req.id, Configs.addressEncoder.fromProposition(ticketContract.getErgoTree).get.toString)
      donateReqDAO.updateStateById(req.id, 1)
      //val txId = ctx.sendTransaction(signedTx)
      println("Donate Tx created with id:  " + signedTx.getId)
    })
  }

  def isReady(req: DonateReq): Boolean = {
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000
    client.getClient.execute(ctx => {
      if(req.state == 0){
        val paymentBoxList = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress))
        println(paymentBoxList)
        if (paymentBoxList.size() == 0) return false
//        println("required amount is "+req.ticketPrice * req.ticketCount," price: "+req.ticketPrice)
        for (i <- 0 until paymentBoxList.size()) {
          if (paymentBoxList.get(i).getValue >= req.ticketPrice * req.ticketCount) {
            println("payment box found")
            donateReqDAO.updateTTL(req.id, currentTime + Configs.creationDelay)
            return true
          }
        }
        return false
      }
      else {
        val newTicketList = ctx.getUnspentBoxesFor(Address.create(req.ticketAddress))
        if (newTicketList.size() != 0){
          for (i <- 0 until newTicketList.size()) {
            if (newTicketList.get(i).getTokens.get(0).getId.toString == req.raffleToken &&
                newTicketList.get(i).getRegisters.get(2).toString == req.participantAddress) {
              donateReqDAO.updateStateById(req.id, 2)
              return false
            }
          }
        }
        val paymentBoxList = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress))
        println(paymentBoxList)
        if (paymentBoxList.size() == 0) return false
        for (i <- 0 until paymentBoxList.size()) {
          if (paymentBoxList.get(i).getValue >= req.ticketPrice* req.ticketCount) {
            println("payment box found")
            donateReqDAO.updateTTL(req.id, currentTime + Configs.creationDelay)
            return true
          }
        }
        return false
      }
    })
  }

  def update(req: DonateReq): Unit ={
    createDonateTx(req)
  }

  // TODO: Chaining
  def isValid(req: DonateReq): Boolean ={
    false
  }

  def nextStage(req: DonateReq): Unit ={
    client.getClient.execute(ctx => {
        val signedCreationTx = ctx.signedTxFromJson(utils.show(req.signedDonateTxJson))
        val txId = ctx.sendTransaction(signedCreationTx)
        println("Donate Transaction Sent with TxId: ", txId)
    })
  }
}
