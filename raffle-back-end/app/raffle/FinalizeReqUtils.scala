package raffle

import helpers.{Configs, Utils}
import io.circe.Json
import models.RefundReq
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit._
import special.collection.{Coll, CollOverArray}

import javax.inject.Inject
import scala.collection.JavaConverters._


class FinalizeReqUtils @Inject()(client: Client, explorer: Explorer,
                                 addresses: Addresses, utils: Utils) {

  /** ******************************* SUCESS FUNCTIONS ***************************************** */
  def completeRaffle(ctx: BlockchainContext, raffleBox: InputBox): SignedTransaction = {
    // first box is raffle winner box
    // second box is charity box
    // third box is raffle service fee
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.serviceSecret)
      .build()
    val boxId = explorer.getUnspentTokenBoxes(
      Configs.token.oracle,
      0,
      100
    ).hcursor.downField("items").as[List[Json]].getOrElse(
      throw new Throwable("invalid explorer response")
    ).head.hcursor.downField("boxId").as[String].getOrElse("")
    val box = ctx.getBoxesById(boxId).head
    val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray
    val charity = r4(0)
    val service = r4(1)
    val ticketPrice = r4(2)
    val totalSoldTicket = r4(5)
    val winBytes = box.getId.getBytes.slice(0, 15)
    var winNumber = BigInt(winBytes)
    val totalSoldTicketBigInt = BigInt(totalSoldTicket)
    winNumber = (winNumber + totalSoldTicketBigInt) % totalSoldTicketBigInt
    val winNumberLong = winNumber.bigInteger.longValue()
    val totalEarning = ticketPrice * totalSoldTicket
    val charityAmount = charity * totalEarning / 100
    val serviceAmount = service * totalEarning / 100
    val charityAddress = utils.getAddress(raffleBox.getRegisters.get(1).getValue.asInstanceOf[Coll[Byte]].toArray)
    val serviceAddress = utils.getAddress(raffleBox.getRegisters.get(3).getValue.asInstanceOf[Coll[Byte]].toArray)
    val winnerAmount = raffleBox.getValue - charityAmount - serviceAmount - Configs.fee
    val newRaffleBox = txB.outBoxBuilder()
      .value(winnerAmount)
      .contract(addresses.getRaffleWinnerContract())
      .tokens(raffleBox.getTokens.get(0), raffleBox.getTokens.get(1))
      .registers(
        utils.longListToErgoValue(r4),
        raffleBox.getRegisters.get(1),
        raffleBox.getRegisters.get(2),
        raffleBox.getRegisters.get(3),
        ErgoValue.of(winNumberLong)
      )
      .build()
    val charityBox = txB.outBoxBuilder()
      .value(charityAmount)
      .contract(new ErgoTreeContract(charityAddress.script))
      .build()
    val serviceBox = txB.outBoxBuilder()
      .value(serviceAmount)
      .contract(new ErgoTreeContract(serviceAddress.script))
      .build()
    val tx = txB.boxesToSpend(Seq(raffleBox).asJava)
      .fee(Configs.fee)
      .outputs(newRaffleBox, charityBox, serviceBox)
      .sendChangeTo(Configs.raffleProjectAddress.getErgoAddress)
      .withDataInputs(Seq(box).asJava)
      .build()
    prover.sign(tx)
  }

  def getWinner(ctx: BlockchainContext, serviceToken: String, winNumber: Long): InputBox = {
    var result: InputBox = null
    var remain = true
    var offset = 0
    while (result == null && remain) {
      val boxes = explorer.getUnspentTokenBoxes(serviceToken, offset, 100).hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("test"))
      val filteredBoxes = boxes.filter(box => {
        try {
          val r5Hex = box.hcursor.downField("additionalRegisters").as[Json].getOrElse(null).hcursor.downField("R5").as[Json].getOrElse(null).hcursor.downField("serializedValue").as[String].getOrElse("")
          val r5 = ErgoValue.fromHex(r5Hex).getValue.asInstanceOf[Coll[Long]].toArray
          r5(0) < winNumber && r5(1) >= winNumber
        } catch {
          case _: Throwable => false
        }
      })
      offset += 100
      remain = boxes.nonEmpty
      if (filteredBoxes.nonEmpty) {
        result = ctx.getBoxesById(filteredBoxes.head.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("box not found"))).head
      }
    }
    result
  }

  def withdrawReward(ctx: BlockchainContext, serviceBox: InputBox, raffleBox: InputBox): SignedTransaction = {
    val winnerTicketBox = getWinner(ctx, raffleBox.getTokens.get(1).getId.toString, raffleBox.getRegisters.get(5).getValue.asInstanceOf[Long])
    val winnerAddress = utils.getAddress(winnerTicketBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toArray)
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.serviceSecret)
      .build()
    val serviceOutput = txB.outBoxBuilder()
      .value(serviceBox.getValue)
      .contract(addresses.getRaffleServiceContract())
      .tokens(
        new ErgoToken(Configs.token.nft, 1),
        new ErgoToken(Configs.token.service, serviceBox.getTokens.get(1).getValue + raffleBox.getTokens.get(0).getValue)
      )
      .registers(
        serviceBox.getRegisters.get(0),
        serviceBox.getRegisters.get(1),
      ).build()
    val winnerOutput = txB.outBoxBuilder()
      .value(raffleBox.getValue)
      .contract(new ErgoTreeContract(winnerAddress.script))
      .build()
    val tx = txB.boxesToSpend(Seq(serviceBox, raffleBox, winnerTicketBox).asJava)
      .fee(Configs.fee)
      .tokensToBurn(
        new ErgoToken(raffleBox.getTokens.get(1).getId, raffleBox.getTokens.get(1).getValue + winnerTicketBox.getTokens.get(0).getValue)
      )
      .sendChangeTo(Configs.raffleProjectAddress.getErgoAddress)
      .outputs(serviceOutput, winnerOutput)
      .build()
    prover.sign(tx)
  }

  /** ******************************* WINNER FUNCTIONS ***************************************** */
  def failRaffle(ctx: BlockchainContext, raffleBox: InputBox): SignedTransaction = {
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.serviceSecret)
      .build()
    val raffleOutput = txB.outBoxBuilder()
      .value(raffleBox.getValue - Configs.fee)
      .tokens(raffleBox.getTokens.get(0), raffleBox.getTokens.get(1))
      .contract(addresses.getRaffleRedeemContract())
      .registers(
        raffleBox.getRegisters.get(0),
        raffleBox.getRegisters.get(1),
        raffleBox.getRegisters.get(2),
        raffleBox.getRegisters.get(3),
      ).build()
    val tx = txB.boxesToSpend(Seq(raffleBox).asJava)
      .fee(Configs.fee)
      .outputs(raffleOutput)
      .sendChangeTo(Configs.serviceAddress.getErgoAddress)
      .build()
    prover.sign(tx)
  }

  def refundRaffle(ctx: BlockchainContext, raffleBox: InputBox, donation: InputBox): SignedTransaction = {
    val refundAddress = utils.getAddress(donation.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toArray)
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.serviceSecret)
      .build()
    val raffleR4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray.clone()
    val ticketPrice = raffleR4(2)
    val refundValue = ticketPrice * donation.getTokens.get(0).getValue
    raffleR4.update(5, raffleR4(5) - donation.getTokens.get(0).getValue)
    val raffleOutput = txB.outBoxBuilder()
      .value(raffleBox.getValue - refundValue)
      .tokens(
        raffleBox.getTokens.get(0),
        new ErgoToken(raffleBox.getTokens.get(1).getId, raffleBox.getTokens.get(1).getValue + donation.getTokens.get(0).getValue)
      )
      .contract(addresses.getRaffleRedeemContract())
      .registers(
        utils.longListToErgoValue(raffleR4),
        raffleBox.getRegisters.get(1),
        raffleBox.getRegisters.get(2),
        raffleBox.getRegisters.get(3),
      ).build()
    val donationRefund = txB.outBoxBuilder()
      .value(refundValue - Configs.fee)
      .contract(new ErgoTreeContract(refundAddress.script))
      .build()
    val tx = txB.boxesToSpend(Seq(raffleBox, donation).asJava)
      .fee(Configs.fee)
      .outputs(raffleOutput, donationRefund)
      .sendChangeTo(Configs.serviceAddress.getErgoAddress)
      .build()
    prover.sign(tx)
  }

  def redeemFailedRaffleToken(ctx: BlockchainContext, serviceBox: InputBox, raffleBox: InputBox): SignedTransaction = {
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
      .withDLogSecret(Configs.serviceSecret)
      .build()
    val serviceOut = txB.outBoxBuilder()
      .value(serviceBox.getValue)
      .contract(addresses.getRaffleServiceContract())
      .tokens(
        serviceBox.getTokens.get(0),
        new ErgoToken(serviceBox.getTokens.get(1).getId, serviceBox.getTokens.get(1).getValue + raffleBox.getTokens.get(0).getValue)
      )
      .registers(
        serviceBox.getRegisters.get(0),
        serviceBox.getRegisters.get(1),
      ).build()
    val tx = txB.boxesToSpend(Seq(serviceBox, raffleBox).asJava)
      .fee(Configs.fee)
      .outputs(serviceOut)
      .tokensToBurn(new ErgoToken(raffleBox.getTokens.get(1).getId, raffleBox.getTokens.get(1).getValue))
      .sendChangeTo(Configs.serviceAddress.getErgoAddress)
      .build()
    prover.sign(tx)
  }

  def processRefundRaffle(ctx: BlockchainContext, raffle: InputBox): Boolean = {
    var remain = true
    var offset = 0
    var newRaffle = raffle
    while (remain) {
      val boxes = explorer.getUnspentTokenBoxes(
        newRaffle.getTokens.get(1).getId.toString,
        offset,
        100
      ).hcursor.downField("items").as[List[Json]].getOrElse(throw new Throwable("test"))
      boxes.filter(ticket => {
        ticket.hcursor.downField("address").as[String].getOrElse("") == Configs.addressEncoder.fromProposition(addresses.getTicketContract().getErgoTree).get.toString
      }).foreach(ticket => {
        val donationBox = ctx.getBoxesById(ticket.hcursor.downField("boxId").as[String].getOrElse(throw new Throwable("invalid ticket found"))).head
        val refundTx = refundRaffle(ctx, newRaffle, donationBox)
        ctx.sendTransaction(refundTx)
        newRaffle = refundTx.getOutputsToSpend.get(0)
      })
      offset += 100
      remain = boxes.nonEmpty
    }
    val tx = ctx.sendTransaction(redeemFailedRaffleToken(ctx, utils.getServiceBox(), raffle))
    true
  }

  /** ******************************* GROUP PROCESS FUNCTIONS ***************************************** */

  def processCompletedRaffle(ctx: BlockchainContext, raffle: InputBox): Boolean = {
    val tx = completeRaffle(ctx, raffle)
    ctx.sendTransaction(tx)
    val tx2 = withdrawReward(ctx, utils.getServiceBox(), tx.getOutputsToSpend.get(0))
    ctx.sendTransaction(tx2)
    true
  }

  def processFailedRaffle(ctx: BlockchainContext, raffle: InputBox): Boolean = {
    val failTx = failRaffle(ctx, raffle)
    ctx.sendTransaction(failTx)
    processRefundRaffle(ctx, failTx.getOutputsToSpend.get(0))
    true
  }

  def processSingleRaffle(ctx: BlockchainContext, raffle: InputBox): Boolean = {
    try {
      val r4 = raffle.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
      val ticketPrice = r4(2)
      val goal = r4(3)
      val totalSoldTicket = r4(5)
      val isSuccess = (totalSoldTicket * ticketPrice >= goal)
      if (isSuccess) {
        processCompletedRaffle(ctx, raffle)
      } else {
        processFailedRaffle(ctx, raffle)
      }
    } catch {
      case tr: Throwable => false
    }
  }

  def processActiveRaffles(ctx: BlockchainContext): Unit = {
    var remain = true
    var offset = 0
    while (remain) {
      remain = false
      ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(addresses.getRaffleActiveContract().getErgoTree).get.toString), offset, 100).asScala.filter(box => {
        remain = true
        box.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray(4) < ctx.getHeight
      }).map(raffle => this.processSingleRaffle(ctx, raffle))
      offset += 100
    }
  }

  def processRefundRaffles(ctx: BlockchainContext): Unit = {
    var remain = true
    var offset = 0
    while (remain) {
      remain = ctx.getUnspentBoxesFor(
        Address.create(Configs.addressEncoder.fromProposition(addresses.getRaffleRedeemContract().getErgoTree).get.toString),
        offset,
        100
      ).asScala.map(raffle => {
        if (!utils.isBoxInMemPool(raffle, 1)) {
          processRefundRaffle(ctx, raffle)
        } else {
          false
        }
      }).nonEmpty
      offset += 100
    }
  }

  def processWinnerRaffle(ctx: BlockchainContext): Unit = {
    var remain = true
    var offset = 0
    var serviceBox = utils.getServiceBox()
    while (remain) {
      remain = ctx.getUnspentBoxesFor(Address.create(Configs.addressEncoder.fromProposition(addresses.getRaffleWinnerContract().getErgoTree).get.toString), offset, 100).asScala.map(raffle => {
        if (!utils.isBoxInMemPool(raffle, 1)) {
          val tx = withdrawReward(ctx, serviceBox, raffle)
          ctx.sendTransaction(tx)
          serviceBox = tx.getOutputsToSpend.get(0)
        }
        true
      }).nonEmpty
      offset += 100
    }
  }

  def Refund(): Unit = {
    client.getClient.execute((ctx: BlockchainContext) => {
      processActiveRaffles(ctx)
      processRefundRaffles(ctx)
      processWinnerRaffle(ctx)
    })
  }
}
