package raffle

import java.util.Calendar

import dao.CreateReqDAO
import helpers.{Configs, Utils}
import io.circe.{Json, parser}
import network.{Client, Explorer}
import javax.inject.Inject
import models.CreateReq
import org.ergoplatform.appkit.impl.{ErgoTreeContract, InputBoxImpl}
import org.ergoplatform.appkit.{Address, ConstantsBuilder, ErgoId, ErgoToken, ErgoValue, InputBox, JavaHelpers}
import scorex.crypto.hash.Digest32
import io.circe.syntax._

import scala.collection.JavaConverters._
import scala.collection.mutable.Seq
import scala.util.control.Breaks.{break, breakable}

class CreateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract,
                               createReqDAO: CreateReqDAO){
  var serviceBox: InputBox = _

  def updateServiceBox() = {
    client.getClient.execute(ctx => {
      val listBoxes = ctx.getUnspentBoxesFor(Configs.serviceAddress)
      println(listBoxes.size)
      for (i <- 0 until listBoxes.size()) {
        if (listBoxes.get(i).getTokens.size() >= 1 && listBoxes.get(i).getTokens.get(0).getId.toString == Configs.serviceTokenId
          && listBoxes.get(i).getTokens.get(0).getValue >= 100000000L) {
          serviceBox = listBoxes.get(i)
        }
      }
    })
  }

  def CreateRaffleProxyAddress(pk: String, winnerPercent: Int, name: String, description: String, deadlineHeight: Long,
                               charityAddr: String, minToRaise: Long, ticketPrice: Long): String = {
    client.getClient.execute(ctx => {

      val winnerContract = ctx.compileContract(
        ConstantsBuilder.create()
          .build(),
        raffleContract.winnerScript)

      val winnerErgoTree = winnerContract.getErgoTree
      val winnerScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(winnerErgoTree.bytes)

      val ticketContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("deadlineHeight", deadlineHeight)
          .item("winnerScriptHash", winnerScriptHash)
          .item("ticketPrice", 1000000L)
          .item("projectPubKey", Configs.raffleProjectAddress.getPublicKey)
          .build(),
        raffleContract.ticketScript)

      val ticketErgoTree = ticketContract.getErgoTree
      val ticketScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(ticketErgoTree.bytes)

      val newRaffleContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("ticketPrice", ticketPrice)
          .item("minToRaise", minToRaise)
          .item("deadlineHeight", deadlineHeight)
          .item("charityPubKey", Address.create(charityAddr).getPublicKey)
          .item("servicePubKey", Configs.serviceAddress.getPublicKey)
          .item("winnerScriptHash", winnerScriptHash)
          .item("minFee", 1000000L)
          .item("ticketScriptHash", ticketScriptHash)
          .item("oracleNebulaNFT", ErgoId.create(Configs.oracleId).getBytes)
          .build(),
        raffleContract.tokenRepoScript)
      val raffleErgoTree = newRaffleContract.getErgoTree
      val raffleScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(raffleErgoTree.bytes)
      val raffleAddress = Configs.addressEncoder.fromProposition(raffleErgoTree).get.toString

      val proxyContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("serviceToken", ErgoId.create(Configs.serviceTokenId).getBytes)
          .item("raffleHash", raffleScriptHash)
          .build(),
        raffleContract.newRaffleProxyScript)
      val proxyScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(proxyContract.getErgoTree.bytes)
      val proxyAddress = Configs.addressEncoder.fromProposition(proxyContract.getErgoTree).get.toString


      val paymentContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("raffleProxyHash", proxyScriptHash)
          .item("pk", Address.create(pk).getPublicKey)
          .build(),
        raffleContract.rafflePaymentScript)
      val paymentAddress = Configs.addressEncoder.fromProposition(paymentContract.getErgoTree).get.toString

      println("New Raffle Payment Address Created")

      val currentTime = Calendar.getInstance().getTimeInMillis / 1000
      createReqDAO.insert(name, description, minToRaise, deadlineHeight, winnerPercent,
        charityAddr, ticketPrice, 0, paymentAddress, proxyAddress, raffleAddress,
        "not-imp", "not-imp", null, null, -1, false,
        Configs.inf + currentTime, Configs.creationDelay + currentTime)

      return paymentAddress
    })
  }

  def raffleTokenIssue(req: CreateReq): Option[String] = {
    client.getClient.execute(ctx => {

      var paymentBox: InputBox = null
      val paymentBoxList = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress))
      println(paymentBoxList)
      //if (paymentBoxList.size() == 0) return
      for (i <- 0 until paymentBoxList.size()) breakable {
        if(paymentBoxList.get(i).getValue >= Configs.fee * 3) {
          paymentBox = paymentBoxList.get(i)
          break
        }
      }

      val proxyContract = new ErgoTreeContract(Address.create(req.proxyAddress).getErgoAddress.script)

      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val txB = ctx.newTxBuilder()

      val proxyBox = txB.outBoxBuilder()
        .value(Configs.fee * 2)
        .contract(proxyContract)
        .tokens(new ErgoToken(paymentBox.getId, 1000000000000000000L))
        .registers(ErgoValue.of(req.name.getBytes("utf-8")),
          ErgoValue.of(req.description.getBytes("utf-8")), ErgoValue.of(0))
        .build()

      val txProxy = txB.boxesToSpend(Seq(paymentBox).asJava)
        .fee(Configs.fee)
        .outputs(proxyBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedProxyTx = prover.sign(txProxy)
      createReqDAO.updateSignedProxyTx(req.id, signedProxyTx.toJson(false))
      createReqDAO.updateRaffleToken(req.id, paymentBox.getId.toString)
      createReqDAO.updateStateById(req.id, 1)
      println("New Raffle Token Issued with TokenId: ", paymentBox.getId.toString)

      val inProxyBox = proxyBox.convertToInputWith(signedProxyTx.getId, 0.toShort)
      println("Proxy box id in token issue function is: "+ inProxyBox.getId, " With values: "+inProxyBox.getValue)

      return Some(signedProxyTx.toJson(false))
    })
  }

  def addRaffle(req: CreateReq, signedProxyTxJson: Option[String]): Unit = {
    client.getClient.execute(ctx => {

      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val serviceContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("servicePubKey", Configs.serviceAddress.getPublicKey)
          .build(),
        raffleContract.raffleServiceScript)

      updateServiceBox()
      // TODO : Use database queries to chain the service boxes
      val newServiceBox = txB.outBoxBuilder()
        .value(serviceBox.getValue)
        .contract(serviceContract)
        .tokens(new ErgoToken(serviceBox.getTokens.get(0).getId, serviceBox.getTokens.get(0).getValue - 1))
        .build()

      val charityPercent = 100 - Configs.servicePercent - req.winnerPercent

      val name = req.name
      val description = req.description
      val winnerPercent = req.winnerPercent
      val deadline = req.deadlineHeight
      val charityAdd = req.charityAdd
      val minTo = req.minToRaise
      val ticketPrice = req.ticketPrice

      val R7: String =
        s"""{
           |  "name" : "$name",
           |  "description" : "$description",
           |  "winnerPercent" : $winnerPercent,
           |  "charityPercent" : $charityPercent,
           |  "deadlineHeight" : $deadline,
           |  "charityAddr" : "$charityAdd",
           |  "minToRaise" : $minTo,
           |  "ticketPrice" : $ticketPrice
           |}""".stripMargin

      //val signedProxyTx = ctx.signedTxFromJson(utils.show(req.signedProxyTxJson))
      var inProxyBox : InputBox = null
      if(utils.show(signedProxyTxJson) == "?"){
        val proxyBoxList = ctx.getUnspentBoxesFor(Address.create(req.proxyAddress))
        //if (proxyBoxList.size() == 0)
        inProxyBox = proxyBoxList.get(0)
        createReqDAO.updateRaffleToken(req.id, inProxyBox.getTokens.get(0).getId.toString)
      }
      else {
        val signedProxyTx = ctx.signedTxFromJson(utils.show(signedProxyTxJson))
        inProxyBox = signedProxyTx.getOutputsToSpend.get(0)
        println("Proxy box id in raffle add function is: " + inProxyBox.getId, " With values: "+inProxyBox.getValue)
      }

      val newRaffleBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(new ErgoTreeContract(Address.create(req.raffleAddress).getErgoAddress.script))
        .tokens(new ErgoToken(inProxyBox.getTokens.get(0).getId, inProxyBox.getTokens.get(0).getValue),
          new ErgoToken(serviceBox.getTokens.get(0).getId, 1))
        .registers(ErgoValue.of(0.toLong), ErgoValue.of(90L - req.winnerPercent), ErgoValue.of(10L),
          ErgoValue.of(R7.getBytes("utf-8")))
        .build()

      //      println("tokenServiceBox: " + serviceBox.getTokens.get(0).getValue)
      //      println("tokenServiceBox: " + serviceBox.getTokens.get(0).getId)
      val tx = txB.boxesToSpend(Seq(serviceBox, inProxyBox).asJava)
        .fee(Configs.fee)
        .outputs(newServiceBox, newRaffleBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      println(tx.toString)

      val signedCreationTx = prover.sign(tx)

      createReqDAO.updateSignedCreationTx(req.id, signedCreationTx.toJson(false))
      val nextServiceBox = newServiceBox.convertToInputWith(signedCreationTx.getId, 0.toShort)
      createReqDAO.updateServiceBoxId(req.id, nextServiceBox.getId.toString)

      println("Creation Tx with id:  " + signedCreationTx.getId)
    })
  }

  // TODO: ONLY REMOVE THE REQUEST IF PAYMENT BOX IS UNAVAILABLE
  def isReady(req: CreateReq): Boolean ={
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000
    client.getClient.execute(ctx => {
      if (req.state == 0) {
        val paymentBoxList = ctx.getUnspentBoxesFor(Address.create(req.paymentAddress))
        println(paymentBoxList)
        if (paymentBoxList.size() == 0) return false
        for (i <- 0 until paymentBoxList.size()) {
          if(paymentBoxList.get(i).getValue >= Configs.fee * 3) {
            println("payment box found")
            createReqDAO.updateTTL(req.id, currentTime + Configs.creationDelay)
            val proxyBoxList = ctx.getUnspentBoxesFor(Address.create(req.proxyAddress))
            if (proxyBoxList.size() == 0) return true
            createReqDAO.updateStateById(req.id, 2)
            createReqDAO.updateTTL(req.id, currentTime + Configs.inf)
            return true
          }
        }
        return false
      }
      else if (req.state == 1) {
        val proxyBoxList = ctx.getUnspentBoxesFor(Address.create(req.proxyAddress))
        if (proxyBoxList.size() != 0) {
          createReqDAO.updateStateById(req.id, 2)
          createReqDAO.updateTTL(req.id, Configs.inf)
          return true
        }

        val newRaffleList = ctx.getUnspentBoxesFor(Address.create(req.raffleAddress))
        if (newRaffleList.size() == 0) return false
        for (i <- 0 until newRaffleList.size()) {
          if(newRaffleList.get(i).getTokens.get(0).getId.toString == req.raffleToken) {
            createReqDAO.updateStateById(req.id, 3)
            return false
          }
        }
      }
      else if (req.state == 2) {
        val newRaffleList = ctx.getUnspentBoxesFor(Address.create(req.raffleAddress))
        if (newRaffleList.size() == 0) return false
        println("Our Raffle Token id should be: "+req.raffleToken)
        for (i <- 0 until newRaffleList.size()) {
          println("Raffle box found with token id: "+newRaffleList.get(i).getTokens.get(0).getId.toString)
          if(newRaffleList.get(i).getTokens.get(0).getId.toString == req.raffleToken) {
            createReqDAO.updateStateById(req.id, 3)
            return false
          }
        }
      }
      return false
    })
  }

  def isValid(req: CreateReq): Boolean = {
    if (req.state == 0) return false
    else if (req.state == 1 || req.state == 2){
      // TODO: Update the code to query the database and check chained service box
      //      if (req.newServiceBox == serviceBox.getId.toString) return true
      //      println("Service Box have Changed, Transaction should be created with new box.")
      //      println("new service box: ", relatedServiceBox, "old service box: ", serviceBox)
      return false
    }
    false
  }

  def update(req: CreateReq): Unit ={
    println("updating the request")
    if (req.state == 0) {
      val signedTxJson : Option[String] = raffleTokenIssue(req)
      addRaffle(req, signedTxJson)
    }
    else if(req.state == 1 || req.state == 2){
      client.getClient.execute(ctx => {
        addRaffle(req, req.signedProxyTxJson)
      })
    }
  }

  def nextStage(req: CreateReq): Int ={
    client.getClient.execute(ctx => {
      if (req.state == 0 || req.state == 1) {
        val signedProxyTx = ctx.signedTxFromJson(utils.show(req.signedProxyTxJson))
        val txId = ctx.sendTransaction(signedProxyTx)
        println("Proxy Transaction Sent with TxId: ", txId)
        val signedCreationTx = ctx.signedTxFromJson(utils.show(req.signedCreateTxJson))
        val txId2 = ctx.sendTransaction(signedCreationTx)
        println("Creation Transaction Sent with TxId: ", txId2)
        return 0
      }
      else if(req.state == 2){
        val signedCreationTx = ctx.signedTxFromJson(utils.show(req.signedCreateTxJson))
        val txId = ctx.sendTransaction(signedCreationTx)
        println("Creation Transaction Sent with TxId: ", txId)
        return 0
      }
      else return 1
    })
  }

}
