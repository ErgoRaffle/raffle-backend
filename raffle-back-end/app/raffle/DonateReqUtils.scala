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
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ErgoId, ErgoToken, ErgoValue, InputBox}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import scorex.crypto.hash.Digest32
import special.collection.{Coll, CollOverArray}
import ContractTypeEnum._
import play.api.Logger

import scala.collection.mutable.Seq
import scala.collection.JavaConverters._
import scala.util.Try

class DonateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract,
                               donateReqDAO: DonateReqDAO, addresses: Addresses){
  private val logger: Logger = Logger(this.getClass)

  def findRaffleBox(tokenId: String): InputBox ={
    client.getClient.execute((ctx: BlockchainContext) => {
      var raffleBoxId: String = ""
      while(raffleBoxId == ""){
        val raffleBoxJson = explorer.getUnspentTokenBoxes(Configs.token.service, 0, 10)
        raffleBoxId = raffleBoxJson.hcursor.downField("items").as[List[Json]].getOrElse(null)
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null).size > 1)
          .filter(_.hcursor.downField("assets").as[Seq[Json]].getOrElse(null)(1)
            .hcursor.downField("tokenId").as[String].getOrElse("") == tokenId).head
          .hcursor.downField("boxId").as[String].getOrElse("")
      }

      var raffleBox = ctx.getBoxesById(raffleBoxId).head
      val serviceAddress = Configs.addressEncoder.fromProposition(raffleBox.getErgoTree).get.toString
      val mempool = explorer.getAddressMempoolTransactions(serviceAddress)
      try {
        val txs = mempool.hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("bad request"))
        var txMap: Map[String, Json] = Map()
        txs.foreach(txJson => {
          val txServiceInput = txJson.hcursor.downField("inputs").as[List[Json]].getOrElse(throw new Throwable("bad response from explorer")).head
          val id = txServiceInput.hcursor.downField("boxId").as[String].getOrElse("")
          txMap += (id -> txJson)
        })
        val keys = txMap.keys.toSeq
        while (keys.contains(raffleBox.getId.toString)) {
          val tmpTx = ctx.signedTxFromJson(txMap(raffleBox.getId.toString).toString())
          raffleBox = tmpTx.getOutputsToSpend.get(0)
        }
      } catch {
        case e: Throwable => logger.error(e.toString)
      }
      raffleBox
    })
  }

  def findProxyAddress(pk: String, raffleId: String, ticketCounts: Long): (String, Long) = {
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

      val raffleBox = findRaffleBox(raffleId)

      logger.debug("Donate payment address created")
      val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray.clone()
      val ticketPrice = r4(2)
      val fee = (ticketPrice * ticketCounts) + (Configs.fee * 2)

      val currentTime = Calendar.getInstance().getTimeInMillis / 1000
      donateReqDAO.insert(ticketCounts, fee, 0, feeEmissionAddress.toString, "nothing", raffleId,
        null, pk, currentTime + Configs.inf, currentTime + Configs.creationDelay)

      return (feeEmissionAddress.toString, fee)
    })
  }

  def createDonateTx(req: DonateReq): Unit = {
    client.getClient.execute(ctx => {
      val raffleBox = findRaffleBox(req.raffleToken)
      val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray.clone()
      val ticketPrice: Long = r4(2)

      val paymentBoxList = ctx.getCoveringBoxesFor(Address.create(req.paymentAddress), req.fee)
      logger.debug(paymentBoxList.getCoveredAmount.toString +" "+ paymentBoxList.isCovered.toString)
      if(!paymentBoxList.isCovered) return

      val txB = ctx.newTxBuilder()
      val deadlineHeight = r4(4)
      val ticketSold = r4(5)
      val total_erg = req.ticketCount * ticketPrice
      r4.update(5, ticketSold + req.ticketCount)

      val outputRaffle = txB.outBoxBuilder()
        .value(raffleBox.getValue + total_erg)
        .contract(addresses.getRaffleActiveContract())
        .tokens(
          raffleBox.getTokens.get(0),
          new ErgoToken(raffleBox.getTokens.get(1).getId, raffleBox.getTokens.get(1).getValue - req.ticketCount)
        )
        .registers(
          utils.longListToErgoValue(r4),
          raffleBox.getRegisters.get(1),
          raffleBox.getRegisters.get(2),
          raffleBox.getRegisters.get(3)
        ).build()

      val ticketOutput = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(addresses.getTicketContract())
        .tokens(
          new ErgoToken(raffleBox.getTokens.get(1).getId, req.ticketCount)
        )
        .registers(
          ErgoValue.of(new ErgoTreeContract(Address.create(req.participantAddress).getErgoAddress.script).getErgoTree.bytes),
          utils.longListToErgoValue(Array(ticketSold, ticketSold + req.ticketCount, deadlineHeight, ticketPrice))
        ).build()

      val txBoxList: Seq[InputBox] = Seq(raffleBox) ++ paymentBoxList.getBoxes.asScala.toSeq
      val tx = txB.boxesToSpend(txBoxList.asJava)
        .fee(Configs.fee)
        .outputs(outputRaffle, ticketOutput)
        .sendChangeTo(Address.create(req.participantAddress).getErgoAddress)
        .build()

      val prover = ctx.newProverBuilder()
        .build()

      val signedTx = prover.sign(tx)

      logger.debug("Donate Tx created with id: "+ signedTx.getId)
      val txId = ctx.sendTransaction(signedTx)
      logger.info("Donate Transaction Sent with TxId: "+ txId)
      donateReqDAO.updateDonateTxId(req.id, signedTx.getId)
      donateReqDAO.updateStateById(req.id, 1)
    })
  }

  def isReady(req: DonateReq): Boolean = {
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000
    client.getClient.execute(ctx => {
      logger.debug("Request state : "+ req.state.toString)
      if (req.state == 0) {
        val paymentBoxList = ctx.getCoveringBoxesFor(Address.create(req.paymentAddress), req.fee)
        logger.debug(paymentBoxList.getCoveredAmount.toString +", "+ req.fee.toString)
        if(paymentBoxList.isCovered) {
          logger.debug("payment box found")
          donateReqDAO.updateTTL(req.id, currentTime + Configs.creationDelay)
          return true
        }
      }
      else {
        if(checkTransaction(req.donateTxID.getOrElse("")) == 1){
          donateReqDAO.updateStateById(req.id, 2)
        }
      }
    })
    false
  }

  def checkTransaction(txId: String): Int = {
    if(txId != "") {
      val unconfirmedTx = explorer.getUnconfirmedTx(txId)
      if (unconfirmedTx == Json.Null) {
        val confirmedTx = explorer.getConfirmedTx(txId)
        if (confirmedTx == Json.Null) {
          0 // resend transaction
        } else {
          1 // transaction mined
        }
      } else {
        2 // transaction already in mempool
      }
    }else{
      0
    }
  }
}
