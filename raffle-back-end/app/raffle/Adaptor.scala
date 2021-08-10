package raffle

import java.io.{PrintWriter, StringWriter}
import java.util.concurrent.ScheduledThreadPoolExecutor
import java.util.concurrent.TimeUnit
import helpers.Configs.addressEncoder
import org.ergoplatform.ErgoAddress
import helpers.{Configs, Utils}
import io.circe.Json
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit._
import play.api.mvc.{AnyContent, Request, Result}
import scorex.crypto.hash.Digest32
import special.collection.Coll
import org.ergoplatform.appkit._
import org.ergoplatform.appkit.impl.ErgoTreeContract
import scorex.crypto.hash.Digest32
import sigmastate.Values.ErgoTree

import java.nio.charset.StandardCharsets
import javax.inject.Inject
import scala.collection.JavaConverters._

class Adaptor @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract) {
  var serviceBox: InputBox = _

  def updateServiceBox() = {
    client.getClient.execute(ctx => {
      val listBoxes = ctx.getUnspentBoxesFor(Configs.serviceAddress)
      println(listBoxes.size)
      for (i <- 0 to listBoxes.size() - 1) {
        if (listBoxes.get(i).getTokens.size() >= 1 && listBoxes.get(i).getTokens.get(0).getId.toString == Configs.serviceTokenId
          && listBoxes.get(i).getTokens.get(0).getValue >= 100000000L) {
          serviceBox = listBoxes.get(i)
        }
      }
    })
  }



  def createService(): String = {
    var txId: String = ""
    client.getClient.execute(ctx => {
      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val txB = ctx.newTxBuilder()
      val serviceContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("servicePubKey", Configs.serviceAddress.getPublicKey)
          .build(),
        raffleContract.raffleServiceScript)
      val listBoxes = ctx.getUnspentBoxesFor(Configs.serviceAddress)
      for (i <- 0 to listBoxes.size() - 1) {
        if (listBoxes.get(i).getValue >= 9000000000L) {
          serviceBox = listBoxes.get(i)
        }
      }
      println("serviceBox value: " + serviceBox.getValue)
      val out = txB.outBoxBuilder()
        .contract(serviceContract)
        .tokens(new ErgoToken(serviceBox.getId, 1000000000000000000L))
        .value(serviceBox.getValue - 10000000L)
        .build()

      val tx = txB.boxesToSpend(Seq(serviceBox).asJava)
        .fee(10000000L)
        .outputs(out)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTx = prover.sign(tx)

      txId = ctx.sendTransaction(signedTx)

      println("tokenId: " + serviceBox.getId)
    })
    return txId
  }



  /*
  * adds a raffle
  * @param name
  * @param description
  * @param deadlineHeight
  * @param organizerAddr
  * @param charityAddr
  * @param minToRaise
  * @return tracnsactionId, in which new raffle added
  * */
  def addRaffle(winnerPercent: Long, name: String, description: String, deadlineHeight: Long, charityAddr: String, minToRaise: Long): String = {
    var txId: String = null
    client.getClient.execute(ctx => {

      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      var txB = ctx.newTxBuilder()
      updateServiceBox()

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
          .item("tokenId", serviceBox.getId.getBytes)
          .item("ticketPrice", 1000000L)
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

      val raffleContractAddress = Configs.addressEncoder.fromProposition(newRaffleContract.getErgoTree).get.toString

      val charityPercent = 100 - Configs.servicePercent - winnerPercent

      val ticketPrice = 1000000L

      val R7: String =
        s"""{
           |  "name" : "$name",
           |  "description" : "$description",
           |  "winnerPercent" : $winnerPercent,
           |  "charityPercent" : $charityPercent,
           |  "deadlineHeight" : $deadlineHeight,
           |  "charityAddr" : "$charityAddr",
           |  "minToRaise" : $minToRaise,
           |  "ticketPrice" : $ticketPrice
           |}""".stripMargin

      val proxyBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(new ErgoTreeContract(Configs.serviceAddress.getErgoAddress.script))
        .tokens(new ErgoToken(serviceBox.getId, 1000000000000000000L))
        .registers(ErgoValue.of(name.getBytes("utf-8")), ErgoValue.of(description.getBytes("utf-8")), ErgoValue.of(0))
        .build()

      val serviceContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("servicePubKey", Configs.serviceAddress.getPublicKey)
          .build(),
        raffleContract.raffleServiceScript)

      val proxyServiceBox = txB.outBoxBuilder()
        .value(serviceBox.getValue - 2 * Configs.fee)
        .contract(serviceContract)
        .tokens(new ErgoToken(serviceBox.getTokens.get(0).getId, serviceBox.getTokens.get(0).getValue))
        .build()

      val txProxy = txB.boxesToSpend(Seq(serviceBox).asJava)
        .fee(Configs.fee)
        .outputs(proxyBox, proxyServiceBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTxProxy = prover.sign(txProxy)
      txB = ctx.newTxBuilder()

      val inProxyBox: InputBox = proxyBox.convertToInputWith(signedTxProxy.getId, 0.toShort)
      val inProxyServiceBox = proxyServiceBox.convertToInputWith(signedTxProxy.getId, 1)
      val newRaffleBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(new ErgoTreeContract(Address.create(raffleContractAddress).getErgoAddress.script))
        .tokens(new ErgoToken(inProxyBox.getTokens.get(0).getId, 1000000000000000000L),
      new ErgoToken(inProxyServiceBox.getTokens.get(0).getId, 1))
        .registers(ErgoValue.of(0.toLong), ErgoValue.of(90L - winnerPercent), ErgoValue.of(10L),
            ErgoValue.of(R7.getBytes("utf-8")))
        .build()

      val newServiceBox = txB.outBoxBuilder()
        .contract(serviceContract)
        .tokens(new ErgoToken(inProxyServiceBox.getTokens.get(0).getId, inProxyServiceBox.getTokens.get(0).getValue - 1))
        .value(inProxyServiceBox.getValue - Configs.fee)
        .build()
      val tokenNewServiceBox = serviceBox.getTokens.get(0).getValue - 1
      println("tokenNewServiceBox: " + tokenNewServiceBox)
      println("tokenServiceBox: " + serviceBox.getTokens.get(0).getValue)
      println("tokenServiceBox: " + serviceBox.getTokens.get(0).getId)
      val tx = txB.boxesToSpend(Seq(inProxyBox, inProxyServiceBox).asJava)
        .fee(Configs.fee)
        .outputs(newRaffleBox, newServiceBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTx = prover.sign(tx)

      println("sign:  " + signedTx.getId)

      val txIdProxy = ctx.sendTransaction(signedTxProxy)
      txId = ctx.sendTransaction(signedTx)
      //      if (!txId.isEmpty) {
      //        serviceBox = newServiceBox.convertToInputWith(txId,1)
      //      }

      println("txIdProxy: " + txIdProxy)
      println(txId)
      println("serviceBox " + serviceBox.getId.toString)
    })
    txId
  }
  def boxRegister(boxId: String): String = {
    client.getClient.execute(ctx => {
      val register = new String(ctx.getBoxesById(boxId)(0).getRegisters.get(3).getValue.asInstanceOf[Coll[Byte]].toArray,
        StandardCharsets.UTF_8)
      register
    })
  }



  def findProxyAddress(pk: String, raffleId: String): String = {
    client.getClient.execute(ctx => {
      val participantAddress : Address = Address.create(pk)
      val propByte = new ErgoTreeContract(participantAddress.getErgoAddress.script)
      val donateContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("tokenId", ErgoId.create(raffleId).getBytes)
          .item("pk", propByte.getErgoTree.bytes)
          .build(),
        raffleContract.donateScript)
      val feeEmissionAddress: ErgoAddress = addressEncoder.fromProposition(donateContract.getErgoTree).get
      return feeEmissionAddress.toString
    })
    return "OK"
  }
  def makeDonateTransDelay(pk: String, proxyAddress: String, raffleId: String): Unit = {
    val exec = new ScheduledThreadPoolExecutor(1)
    exec.schedule(new Runnable() {
      override def run(): Unit = {
        try {
          makeDonateTrans(pk, proxyAddress, raffleId)
        } catch {
          case e: Throwable => print(getStackTraceStr(e))
      }
      }
    }, 1, TimeUnit.SECONDS)
  }

  var raffleBox: InputBox = _
  def makeDonateTrans(pk: String, proxyAddress: String, raffleId: String): String = {
    client.getClient.execute(ctx => {
      println("Start")
      var index = 0
      val end = explorer.getUnspentTokenBoxes(raffleId, 1000000000, 1)
      var box = explorer.getUnspentTokenBoxes(raffleId, index, 1)
      while(box != end) {
        val boxId = box.hcursor.downField("items").as[Array[Json]].getOrElse(throw new Throwable("parse error"))
                    .map(m => m.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("BoxId Not Found")))
        val BOX = ctx.getBoxesById(boxId(0))(0)
        if (BOX.getTokens.size() >= 2 && BOX.getTokens.get(1).getId.toString == Configs.serviceTokenId
          && BOX.getTokens.get(0).getId.toString == raffleId) {
          raffleBox = BOX
        }
        index += 1
        box = explorer.getUnspentTokenBoxes(raffleId, index, 1)
      }
      println("END")
      val proxyBoxList = ctx.getUnspentBoxesFor(Address.create(proxyAddress))
      if (proxyBoxList.size() == 0) return "No Box Found"
      val proxyBox = proxyBoxList.get(0)


      println("END")
      println(raffleBox.getId)
      println(proxyBox.getId)

      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val txB = ctx.newTxBuilder()

      val R4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[Long]
      val R5 = raffleBox.getRegisters.get(1).getValue.asInstanceOf[Long]
      val R6 = raffleBox.getRegisters.get(2).getValue.asInstanceOf[Long]
      val R7 = new String(raffleBox.getRegisters.get(3).getValue.asInstanceOf[Coll[Byte]].toArray,
        StandardCharsets.UTF_8)

      println(R7)
      println("OK")
      val ticketPrice = io.circe.jawn.parse(R7)
        .getOrElse(throw new Throwable("ticket parse error"))
        .hcursor.downField("ticketPrice").as[Long].getOrElse(throw new Throwable("ticketPrice not Found"))

      println("OK")
      val deadlineHeight = io.circe.jawn.parse(R7)
        .getOrElse(throw new Throwable("deadLine parse error"))
        .hcursor.downField("deadlineHeight").as[Long].getOrElse(throw new Throwable("deadLine not Found"))

      println(R7)

      val tickets = (proxyBox.getValue - 2 * Configs.fee) / 1000000
      val newRaffleBox = txB.outBoxBuilder()
        .value(raffleBox.getValue + proxyBox.getValue - 2 * Configs.fee)
        .contract(new ErgoTreeContract(addressEncoder.fromProposition(raffleBox.getErgoTree).get.script))
        .tokens(new ErgoToken(raffleBox.getTokens.get(0).getId,  raffleBox.getTokens.get(0).getValue - tickets),
          new ErgoToken(raffleBox.getTokens.get(1).getId,  raffleBox.getTokens.get(1).getValue))
        .registers(ErgoValue.of((R4 + tickets).toLong) , ErgoValue.of(R5), ErgoValue.of(R6), ErgoValue.of(R7.getBytes("utf-8")))
        .build()

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


      val participantAddress : Address = Address.create(pk)
      val propByte = new ErgoTreeContract(participantAddress.getErgoAddress.script)

      val scriptTokenRepoHash: Digest32 = scorex.crypto.hash.Blake2b256(raffleBox.getErgoTree.bytes)
      val TicketBox = txB.outBoxBuilder()
        .value(Configs.fee)
        .contract(ticketContract)
        .registers(ErgoValue.of(R4.toLong), ErgoValue.of(tickets.toLong), ErgoValue.of(scriptTokenRepoHash),
          ErgoValue.of(propByte.getErgoTree.bytes))
        .tokens(new ErgoToken(raffleBox.getTokens.get(0).getId, tickets))
        .build()

      val tx = txB.boxesToSpend(Seq(raffleBox, proxyBox).asJava)
        .fee(Configs.fee)
        .outputs(newRaffleBox, TicketBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

        val signedTx = prover.sign(tx)
        val txId = ctx.sendTransaction(signedTx)
        println(txId)

    })
    return "OK"
  }
  def getStackTraceStr(e: Throwable): String = {
    val sw = new StringWriter
    val pw = new PrintWriter(sw)
    e.printStackTrace(pw)
    sw.toString
  }
}