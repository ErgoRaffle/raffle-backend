package raffle

import com.google.protobuf.TextFormat.ParseException
import helpers.{Configs, Utils, connectionException, failedTxException, parseException, proveException}
import io.circe.Json
import network.{Client, Explorer}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit._
import special.collection.{Coll, CollOverArray}
import javax.inject.Inject
import play.api.Logger

import scala.collection.JavaConverters._
import scala.util.Try


class FinalizeReqUtils @Inject()(client: Client, explorer: Explorer,
                                 addresses: Addresses, utils: Utils) {
  private val logger: Logger = Logger(this.getClass)

  /** ******************************* SUCESS FUNCTIONS ***************************************** */
  def completeRaffle(ctx: BlockchainContext, raffleBox: InputBox): SignedTransaction = {
    // first box is raffle winner box
    // second box is charity box
    // third box is raffle service fee
    try {
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
        .build()
      val boxId = explorer.getUnspentTokenBoxes(Configs.token.oracle, 0, 100)
        .hcursor.downField("items").as[List[Json]].getOrElse(throw parseException())
        .head.hcursor.downField("boxId").as[String].getOrElse("")
      val box = ctx.getBoxesById(boxId).head
      val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray
      val charity = r4(0)
      val service = r4(1)
      val ticketPrice = r4(2)
      val totalSoldTicket = r4(5)
      val winBytes = box.getId.getBytes.slice(0, 15)
      val winNumber = (((BigInt(winBytes) % totalSoldTicket) + totalSoldTicket) % totalSoldTicket).toLong
      logger.info(s"winner number is ${winNumber} in raffle ${raffleBox.getTokens.get(1).getId}")
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
          ErgoValue.of(winNumber)
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
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .withDataInputs(Seq(box).asJava)
        .build()

      try {
        val signedTx = prover.sign(tx)
        signedTx
      } catch {
        case e: Throwable => {
          logger.error(s"raffle ${raffleBox.getTokens.get(1).getId} final tx proving failed")
          logger.error(utils.getStackTraceStr(e))
          throw proveException()
        }
      }
    }
    catch {
      case _: parseException => throw connectionException()
      case e: ErgoClientException => {
        logger.warn(e.getMessage)
        throw connectionException()
      }
      case e: proveException => throw e
      case e: Throwable => {
        logger.error(s"raffle ${raffleBox.getTokens.get(1).getId} final tx generation failed")
        logger.error(utils.getStackTraceStr(e))
        throw e
      }
    }
  }

  def getWinner(ctx: BlockchainContext, serviceToken: String, winNumber: Long): InputBox = {
    try {
      var result: InputBox = null
      var remain = true
      var offset = 0
      while (result == null && remain) {
        val boxes = explorer.getUnspentTokenBoxes(serviceToken, offset, 100).hcursor.downField("items").as[List[Json]].getOrElse(throw parseException())
        val filteredBoxes = boxes.filter(box => {
          try {
            val r5Hex = box.hcursor.downField("additionalRegisters").as[Json].getOrElse(null).hcursor.downField("R5").as[Json].getOrElse(null)
              .hcursor.downField("serializedValue").as[String].getOrElse("")
            val r5 = ErgoValue.fromHex(r5Hex).getValue.asInstanceOf[Coll[Long]].toArray
            r5(0) <= winNumber && r5(1) > winNumber
          } catch {
            case _: Throwable => false
          }
        })
        offset += 100
        remain = boxes.nonEmpty
        if (filteredBoxes.nonEmpty) {
          result = ctx.getBoxesById(filteredBoxes.head.hcursor.downField("boxId").as[String].getOrElse(throw parseException())).head
        }
      }
      result
    } catch {
      case _: parseException => throw connectionException()
      case e: ErgoClientException => {
        logger.error(e.getMessage)
        throw connectionException()
      }
      case e: Throwable => throw e
    }
  }

  def withdrawReward(ctx: BlockchainContext, serviceBox: InputBox, raffleBox: InputBox): SignedTransaction = {
    try {
      val winnerTicketBox = getWinner(ctx, raffleBox.getTokens.get(1).getId.toString, raffleBox.getRegisters.get(4).getValue.asInstanceOf[Long])
      val winnerAddress = utils.getAddress(winnerTicketBox.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toArray)
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
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
        .sendChangeTo(Configs.serviceAddress.getErgoAddress)
        .outputs(serviceOutput, winnerOutput)
        .build()
      try {
        val signedTx = prover.sign(tx)
        signedTx
      } catch {
        case e: Throwable => {
          logger.error(s"raffle ${raffleBox.getTokens.get(1).getId} winner reward tx proving failed")
          logger.error(utils.getStackTraceStr(e))
          throw proveException()
        }
      }
    } catch {
      case _: connectionException => throw connectionException()
      case e: Throwable => {
        logger.error(s"raffle ${raffleBox.getTokens.get(1).getId.toString} winner reward withdraw tx generation failed")
        logger.error(utils.getStackTraceStr(e))
        throw e
      }
    }
  }

  /** ******************************* FAIL FUNCTIONS ***************************************** */
  def failRaffle(ctx: BlockchainContext, raffleBox: InputBox): SignedTransaction = {
    try {
      val txB = ctx.newTxBuilder()
      val prover = ctx.newProverBuilder()
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
      try {
        val signedTx = prover.sign(tx)
        signedTx
      } catch {
        case e: Throwable => {
          logger.error(s"raffle ${raffleBox.getTokens.get(1).getId} final fail tx proving failed")
          logger.error(utils.getStackTraceStr(e))
          throw proveException()
        }
      }
    } catch {
      case e: proveException => throw e
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        logger.error(s"raffle ${raffleBox.getTokens.get(1).getId} final fail tx generation failed")
        throw e
      }
    }
  }

  def refundRaffle(ctx: BlockchainContext, raffleBox: InputBox, donation: InputBox): SignedTransaction = {
    val refundAddress = utils.getAddress(donation.getRegisters.get(0).getValue.asInstanceOf[Coll[Byte]].toArray)
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
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
      .value(refundValue)
      .contract(new ErgoTreeContract(refundAddress.script))
      .build()
    val tx = txB.boxesToSpend(Seq(raffleBox, donation).asJava)
      .fee(Configs.fee)
      .outputs(raffleOutput, donationRefund)
      .sendChangeTo(Configs.serviceAddress.getErgoAddress)
      .build()
    try {
      val signedTx = prover.sign(tx)
      signedTx
    } catch {
      case e: Throwable => {
        logger.error(s"ticket with boxId ${donation.getId.toString} refund tx proving failed")
        logger.error(utils.getStackTraceStr(e))
        throw proveException()
      }
    }
  }

  def redeemFailedRaffleToken(ctx: BlockchainContext, serviceBox: InputBox, raffleBox: InputBox): SignedTransaction = {
    val txB = ctx.newTxBuilder()
    val prover = ctx.newProverBuilder()
      .build()
    val serviceOut = txB.outBoxBuilder()
      .value(serviceBox.getValue)
      .contract(new ErgoTreeContract(serviceBox.getErgoTree))
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
    try {
      val signedTx = prover.sign(tx)
      signedTx
    } catch {
      case e: Throwable => {
        logger.error(s"raffle ${raffleBox.getTokens.get(1).getId} final redeem tx proving failed")
        logger.error(utils.getStackTraceStr(e))
        throw proveException()
      }
    }
  }

  def processRefundRaffle(ctx: BlockchainContext, raffle: InputBox): Unit = {
    try {
      var remain = true
      var offset = 0
      var newRaffle = raffle
      val raffleAddress = Configs.addressEncoder.fromProposition(newRaffle.getErgoTree).get.toString
      Try {
        newRaffle = utils.findMempoolBox(raffleAddress, newRaffle, ctx)
      }
      while (remain) {
        val boxes = explorer.getUnspentTokenBoxes(newRaffle.getTokens.get(1).getId.toString, offset, 100)
          .hcursor.downField("items").as[List[Json]].getOrElse(throw parseException())

        boxes.filter(ticket => {
          ticket.hcursor.downField("address").as[String].getOrElse("") == Configs.addressEncoder.fromProposition(addresses.getTicketContract().getErgoTree).get.toString
        }).foreach(ticket => {
          try {
            val donationBox = ctx.getBoxesById(ticket.hcursor.downField("boxId").as[String].getOrElse(throw parseException())).head
            if(!utils.isBoxInMemPool(donationBox)) {
              val refundTx = refundRaffle(ctx, newRaffle, donationBox)
              var txId = ctx.sendTransaction(refundTx)
              if (txId == null) throw failedTxException(s"ticket with boxId ${donationBox.getId} in raffle ${raffle.getTokens.get(1).getId.toString} refund tx sending failed")
              else txId = txId.replaceAll("\"", "")
              logger.info(s"ticket with boxId ${donationBox.getId} refunded successfully with txId ${txId}")
              newRaffle = refundTx.getOutputsToSpend.get(0)
            }
          } catch {
            case e: parseException => {
              logger.warn(utils.getStackTraceStr(e))
              throw parseException()
            }
            case _: connectionException =>
            case _: proveException =>
            case e: failedTxException => {
              logger.warn(e.getMessage)
              logger.warn("skipping chain refund transactions")
              throw e
            }
            case e: ErgoClientException => logger.warn(e.getMessage)
            case e: Throwable => logger.error(utils.getStackTraceStr(e))
          }
        })
        offset += 100
        remain = boxes.nonEmpty
      }
      if (raffle.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].getOrElse(5, 100) == 0L) {
        val redeemTx = redeemFailedRaffleToken(ctx, utils.getServiceBox(), raffle)
        var txId = ctx.sendTransaction(redeemTx)
        if (txId == null) throw failedTxException(s"raffle ${raffle.getTokens.get(1).getId} final redeem tx sending failed")
        else txId = txId.replaceAll("\"", "")
        logger.info(s"raffle ${raffle.getTokens.get(1).getId} final redeem tx sent successfully with txId ${txId}")
      }
    } catch {
      case _: connectionException =>
      case e: parseException => logger.warn(e.getMessage)
      case e: ErgoClientException => logger.warn(e.getMessage)
      case e: failedTxException => logger.error(e.getMessage)
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw e
      }
    }
  }

  /** ******************************* GROUP PROCESS FUNCTIONS ***************************************** */

  def processCompletedRaffle(ctx: BlockchainContext, raffle: InputBox): Unit = {
    try {
      val tx = completeRaffle(ctx, raffle)
      var txId = ctx.sendTransaction(tx)
      if (txId == null) throw failedTxException(s"raffle ${raffle.getTokens.get(1).getId.toString} final tx sending failed")
      else txId = txId.replaceAll("\"", "")
      logger.info(s"complete successful raffle Tx: ${txId}")
      val tx2 = withdrawReward(ctx, utils.getServiceBox(), tx.getOutputsToSpend.get(0))
      var txId2 = ctx.sendTransaction(tx2)
      if (txId2 == null) throw failedTxException(s"raffle ${raffle.getTokens.get(1).getId.toString} winner withdraw tx sending failed")
      else txId2 = txId2.replaceAll("\"", "")
      logger.info(s"winner withdraw Tx: ${txId2}")
    } catch {
      case _: connectionException =>
      case e: failedTxException => {
        logger.error(e.getMessage)
      }
      case e: Throwable => {
        logger.warn(e.getMessage)
      }
    }
  }

  def processFailedRaffle(ctx: BlockchainContext, raffle: InputBox): Unit = {
    try {
      val failTx = failRaffle(ctx, raffle)
      var txId = ctx.sendTransaction(failTx)
      if (txId == null) throw failedTxException(s"raffle ${raffle.getTokens.get(1).getId.toString} final tx sending failed")
      else txId = txId.replaceAll("\"", "")
      logger.info(s"fail raffle tx sent with txId ${txId}")
      processRefundRaffle(ctx, failTx.getOutputsToSpend.get(0))
    } catch {
      case _: connectionException =>
      case e: failedTxException => {
        logger.error(e.getMessage)
      }
      case e: Throwable => {
        logger.warn(e.getMessage)
      }
    }
  }

  def processSingleRaffle(ctx: BlockchainContext, raffle: InputBox): Unit = {
    try {
      val r4 = raffle.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray
      val ticketPrice = r4(2)
      val goal = r4(3)
      val totalSoldTicket = r4(5)
      val isSuccess = (totalSoldTicket * ticketPrice >= goal)
      logger.info(s"processing raffle ${raffle.getTokens.get(1).getId} and goal achieving is ${isSuccess}")
      if (isSuccess) {
        processCompletedRaffle(ctx, raffle)
      } else {
        processFailedRaffle(ctx, raffle)
      }
    } catch {
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def processActiveRaffles(ctx: BlockchainContext): Unit = {
    try {
      client.getAllUnspentBox(Address.create(Configs.addressEncoder.fromProposition(addresses.getRaffleActiveContract().getErgoTree).get.toString))
        .filter(box => {
          box.getRegisters.get(0).getValue.asInstanceOf[Coll[Long]].toArray(4) < ctx.getHeight
        }).foreach(raffle => {
        if (!utils.isBoxInMemPool(raffle)) processSingleRaffle(ctx, raffle)
      })
    } catch {
      case e: connectionException => logger.warn(e.getMessage)
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def processRefundRaffles(ctx: BlockchainContext): Unit = {
    try {
      client.getAllUnspentBox(Address.create(Configs.addressEncoder.fromProposition(addresses.getRaffleRedeemContract().getErgoTree).get.toString))
        .foreach(raffle => processRefundRaffle(ctx, raffle))
    }
    catch {
      case _: connectionException =>
      case e: Throwable => logger.error(e.getMessage)
    }
  }

  def processWinnerRaffle(ctx: BlockchainContext): Unit = {
    try {
      var serviceBox = utils.getServiceBox()
      client.getAllUnspentBox(Address.create(Configs.addressEncoder.fromProposition(addresses.getRaffleWinnerContract().getErgoTree).get.toString))
        .foreach(winner => {
          if (!utils.isBoxInMemPool(winner)) {
            val tx = withdrawReward(ctx, serviceBox, winner)
            var txId = ctx.sendTransaction(tx)
            if (txId == null) throw failedTxException(s"winner box ${winner.getTokens.get(1).getId.toString} tx sending failed")
            else txId = txId.replaceAll("\"", "")
            logger.info(s"winner funding tx sent with txId ${txId}")
            serviceBox = tx.getOutputsToSpend.get(0)
          }
      })
    } catch {
      case _: connectionException =>
      case e: Throwable => logger.error(e.getMessage)
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
