package raffle


import javax.inject.Inject
import play.api.Logger

import java.security.SecureRandom
import scala.collection.JavaConverters._
import network.{Client, Explorer}
import sigmastate.interpreter.CryptoConstants.{dlogGroup, groupOrder, secureRandom}
import sigmastate.eval._
import special.sigma.GroupElement
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ContextVar, ErgoId, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, NetworkType, OutBox}
import helpers.{Configs, Utils}
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.impl.ErgoTreeContract
import scorex.crypto.hash.Digest32
import scorex.util.encode.Base58
import sigmastate.Values
import sigmastate.interpreter.CryptoConstants
import special.collection.Coll

import java.math.BigInteger
import java.{lang, util}
import scala.reflect.io.Streamable.Bytes


class Adaptor @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract) {
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
  var serviceBox: InputBox = _

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

      val out = txB.outBoxBuilder()
        .contract(serviceContract)
        .tokens(new ErgoToken(listBoxes.get(0).getId, 1000000000000000000L))
        .value(60000000000L - 1000000L)
        .build()

      val tx = txB.boxesToSpend(listBoxes)
        .fee(1000000)
        .outputs(out)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTx = prover.sign(tx)
      txId = ctx.sendTransaction(signedTx)

      print(txId)
    })
    return txId
  }
  def addRaffle(winnerPercent: Long, name: String, description: String, deadlineHeight: Long, charityAddr: String, minToRaise: Long): String = {
    var txId: String = "a"
    client.getClient.execute(ctx => {

      val prover = ctx.newProverBuilder()
        .withDLogSecret(Configs.serviceSecret)
        .build()

      val txB = ctx.newTxBuilder()
      val listBoxes = ctx.getUnspentBoxesFor(Configs.serviceAddress)
      for (i <- 0 to listBoxes.size() - 1) {
        println(listBoxes.get(i).getTokens.get(0).getId)
        println(listBoxes.get(i).getTokens.get(0).getId)
        println(listBoxes.get(i).getTokens.get(0).getValue)
        if (listBoxes.get(i).getTokens.get(0).getId.toString == raffleContract.serviceTokenId
          && listBoxes.get(i).getTokens.get(0).getValue >= 100000000L) {
          serviceBox = listBoxes.get(i)
          println("a")
        }
      }

      val winnerContract = ctx.compileContract(
        ConstantsBuilder.create()
          .build(),
        raffleContract.winnerScript)

      val winnerErgoTree = winnerContract.getErgoTree
      val winnerScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(winnerErgoTree.bytes)

      val ticketContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("deadlineHeight", 50000000)
          .item("winnerScriptHash", winnerScriptHash)
          .item("ticketPrice", 1000000L)
          .item("projectPubKey", Configs.raffleProjectAddress.getPublicKey)
          .build(),
        raffleContract.ticketScript)

      val ticketErgoTree = ticketContract.getErgoTree
      val ticketScriptHash: Digest32 = scorex.crypto.hash.Blake2b256(ticketErgoTree.bytes)


      println(serviceBox)
      val newRaffleContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("tokenId", serviceBox.getId)
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


      val R7: String =
        s"""{
           |  name : "$name"
           |  description : "$description"
           |}""".stripMargin
      val newRaffleBox = txB.outBoxBuilder()
        .value(1000000L)
        .contract(newRaffleContract)
        .tokens(new ErgoToken(serviceBox.getId, 1000000000000000000L), new ErgoToken(raffleContract.serviceTokenId, 1))
        .registers(ErgoValue.of(0L), ErgoValue.of(90L - winnerPercent), ErgoValue.of(10L), ErgoValue.of(R7.getBytes()))
        .build()


      val serviceContract = ctx.compileContract(
        ConstantsBuilder.create()
          .item("servicePubKey", Configs.serviceAddress.getPublicKey)
          .build(),
        raffleContract.raffleServiceScript)
      val newServiceBox = txB.outBoxBuilder()
        .contract(serviceContract)
        .tokens(new ErgoToken(serviceBox.getTokens.get(0).getId, serviceBox.getTokens.get(0).getValue - 1))
        .value(serviceBox.getValue - 2000000L)
        .build()

      val tx = txB.boxesToSpend(Seq(serviceBox).asJava)
        .fee(1000000)
        .outputs(newRaffleBox, newServiceBox)
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .build()

      val signedTx = prover.sign(tx)
      txId = ctx.sendTransaction(signedTx)
      println(txId)
    })
    txId
  }


}