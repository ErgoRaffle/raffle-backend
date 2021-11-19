package raffle

import java.time.LocalDateTime
import dao.{DonateReqDAO, RaffleCacheDAO}
import helpers.{Configs, Utils, connectionException, failedTxException, finishedRaffleException, internalException, paymentNotCoveredException, proveException}

import javax.inject.Inject
import models.{DonateReq, Raffle}
import network.Client
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.{Address, ErgoClientException, ErgoToken, ErgoValue, InputBox, SignedTransaction}
import special.collection.CollOverArray
import play.api.Logger

import scala.collection.mutable.Seq
import scala.collection.JavaConverters._


class DonateReqUtils @Inject()(client: Client, utils: Utils, donateReqDAO: DonateReqDAO, addresses: Addresses, raffleCacheDAO: RaffleCacheDAO){
  private val logger: Logger = Logger(this.getClass)

  def findProxyAddress(pk: String, raffleId: String, ticketCounts: Long): (String, Long, Long) = {
    try {
      client.getClient.execute(ctx => {
        val raffle = raffleCacheDAO.byTokenId(raffleId)
        val expectedDonate = (raffle.ticketPrice * ticketCounts) + (Configs.fee * 2)
        if (raffle.deadlineHeight < ctx.getHeight) {
          throw finishedRaffleException(s"raffle ${raffleId} has finished can not create proxy address")
        }

        val donateContract = addresses.getRaffleDonateProxyContract(pk, raffleId, ticketCounts, raffle.deadlineHeight)
        val feeEmissionAddress: ErgoAddress = Configs.addressEncoder.fromProposition(donateContract.getErgoTree).get
        val req: DonateReq = donateReqDAO.insert(ticketCounts, expectedDonate, raffle.deadlineHeight, 0, feeEmissionAddress.toString, raffleId,
          null, pk, LocalDateTime.now().toString, client.getHeight + Configs.creationDelay)
        logger.debug("Donate payment address created")

        (feeEmissionAddress.toString, expectedDonate, req.id)
      })
    }
    catch {
      case e: finishedRaffleException => throw e
      case e: connectionException => throw e
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("Error in payment address generation")
      }
    }
  }

  def createDonateTx(req: DonateReq, raffleBox: InputBox): InputBox = {
    try {
      client.getClient.execute(ctx => {
        val raffleInfo = Raffle(raffleBox, utils)

        val paymentBoxCover = utils.getCoveringBoxesWithMempool(req.paymentAddress, req.fee)
        if (!paymentBoxCover._2) throw paymentNotCoveredException(s"Donation payment for request ${req.id} not covered the fee, request state is ${req.state} and request tx is ${req.donateTxID.orNull}")
        logger.debug(s"Payment covered amount for request ${req.id} is ${paymentBoxCover._3}")

        val txB = ctx.newTxBuilder()
        val total_erg = req.ticketCount * raffleInfo.ticketPrice
        val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray.clone()
        r4.update(5, raffleInfo.tickets + req.ticketCount)

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
            ErgoValue.of(Address.create(req.participantAddress).getErgoAddress.script.bytes),
            utils.longListToErgoValue(Array(raffleInfo.tickets, raffleInfo.tickets + req.ticketCount, raffleInfo.deadlineHeight, raffleInfo.ticketPrice))
          ).build()

        var change = paymentBoxCover._3 - req.fee
        var fee = Configs.fee
        if (change <= Configs.minBoxErg) fee += change
        val txBoxList: Seq[InputBox] = Seq(raffleBox) ++ paymentBoxCover._1
        val tx = txB.boxesToSpend(txBoxList.asJava)
          .fee(fee)
          .outputs(outputRaffle, ticketOutput)
          .sendChangeTo(Address.create(req.participantAddress).getErgoAddress)
          .build()

        val prover = ctx.newProverBuilder()
          .build()

        var signedTx: SignedTransaction = null
        try {
          signedTx = prover.sign(tx)
          logger.debug(s"create tx for request ${req.id} proved successfully")
        } catch {
          case e: Throwable =>
            logger.error(utils.getStackTraceStr(e))
            logger.error(s"create tx for request ${req.id} proving failed")
            throw proveException()
        }

        var txId = ctx.sendTransaction(signedTx)
        if (txId == null) throw failedTxException(s"Donataion transaction sending failed for ${req.id}")
        else txId = txId.replaceAll("\"", "")
        logger.info("Donate Transaction Sent with TxId: " + txId)
        donateReqDAO.updateDonateTxId(req.id, txId)
        donateReqDAO.updateStateById(req.id, 1)
        signedTx.getOutputsToSpend.get(0)
      })
    } catch {
      case e: connectionException => throw e
      case _: proveException => throw internalException()
      case e: failedTxException =>
        logger.warn(e.getMessage)
        throw internalException()
      case e: ErgoClientException =>
        logger.warn(e.getMessage)
        throw connectionException()
      case e: paymentNotCoveredException =>
        logger.warn(e.getMessage)
        throw internalException()
      case e: Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw internalException()
    }
  }

  def isReady(req: DonateReq): Boolean = {
    logger.debug("Request state : "+ req.state.toString)
    val paymentBoxCover = utils.getCoveringBoxesWithMempool(req.paymentAddress, req.fee)

    if(paymentBoxCover._2) {
      donateReqDAO.updateTTL(req.id, client.getHeight + Configs.creationDelay)
    }
    if (req.state == 0) {
      if(paymentBoxCover._2) {
        logger.info(s"Payment found for request ${req.id}")
        return true
      }
    }
    else {
      val txState = utils.checkTransaction(req.donateTxID.getOrElse(""))
      if(txState == 1) donateReqDAO.updateStateById(req.id, 2)
      else if(txState == 0) return true
    }
    false
  }
}
