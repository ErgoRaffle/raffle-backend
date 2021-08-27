package raffle

import dao.DonateReqDAO
import helpers.{Configs, Utils}
import javax.inject.Inject
import models.DonateReq
import network.{Client, Explorer}
import org.ergoplatform.ErgoAddress
import org.ergoplatform.appkit.{Address, BlockchainContext, ConstantsBuilder, ErgoId, ErgoToken, ErgoValue, InputBox}
import special.collection.{Coll, CollOverArray}
import play.api.Logger

import scala.collection.mutable.Seq
import scala.collection.JavaConverters._


class DonateReqUtils @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract,
                               donateReqDAO: DonateReqDAO, addresses: Addresses){
  private val logger: Logger = Logger(this.getClass)

  def findProxyAddress(pk: String, raffleId: String, ticketCounts: Long): (String, Long) = {
    try {
      client.getClient.execute(ctx => {
        val raffleBox = utils.getRaffleBox(raffleId)

        val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray.clone()
        val ticketPrice = r4(2)
        val expectedDonate = (ticketPrice * ticketCounts) + (Configs.fee * 2)
        val raffleDeadline = r4(4)
        if (raffleDeadline < ctx.getHeight) {
          throw new Throwable("Raffle is finished")
        }

        val donateContract = ctx.compileContract(
          ConstantsBuilder.create()
            .item("tokenId", ErgoId.create(raffleId).getBytes)
            .item("userAddress", Address.create(pk).getErgoAddress.script.bytes)
            .item("ticketCount", ticketCounts)
            .item("minFee", Configs.fee)
            .item("expectedDonate", expectedDonate)
            .item("raffleDeadline", raffleDeadline)
            .build(),
          raffleContract.donateScript)

        val feeEmissionAddress: ErgoAddress = Configs.addressEncoder.fromProposition(donateContract.getErgoTree).get

        donateReqDAO.insert(ticketCounts, expectedDonate, raffleDeadline, 0, feeEmissionAddress.toString, "nothing", raffleId,
          null, pk, utils.currentTime + Configs.inf, utils.currentTime + Configs.creationDelay)
        logger.debug("Donate payment address created")

        (feeEmissionAddress.toString, expectedDonate)
      })
    }
    catch {
      case e: Throwable => {
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("Error in payment address generation")
      }
    }
  }

  def createDonateTx(req: DonateReq): Unit = {
    try {
      client.getClient.execute(ctx => {
        val raffleBox = utils.getRaffleBox(req.raffleToken)
        val r4 = raffleBox.getRegisters.get(0).getValue.asInstanceOf[CollOverArray[Long]].toArray.clone()
        val ticketPrice: Long = r4(2)

        val paymentBoxList = ctx.getCoveringBoxesFor(Address.create(req.paymentAddress), req.fee)
        logger.debug(paymentBoxList.getCoveredAmount.toString + " " + paymentBoxList.isCovered.toString)
        // TODO: Add ChainTx for paymentBoxes
        if (!paymentBoxList.isCovered) throw new Throwable("Donation payment not covered the fee")

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
            ErgoValue.of(Address.create(req.participantAddress).getErgoAddress.script.bytes),
            utils.longListToErgoValue(Array(ticketSold, ticketSold + req.ticketCount, deadlineHeight, ticketPrice))
          ).build()

        var change = paymentBoxList.getCoveredAmount - req.fee
        var fee = Configs.fee
        if (change <= Configs.minBoxErg) fee += change
        val txBoxList: Seq[InputBox] = Seq(raffleBox) ++ paymentBoxList.getBoxes.asScala.toSeq
        val tx = txB.boxesToSpend(txBoxList.asJava)
          .fee(fee)
          .outputs(outputRaffle, ticketOutput)
          .sendChangeTo(Address.create(req.participantAddress).getErgoAddress)
          .build()

        val prover = ctx.newProverBuilder()
          .build()

        val signedTx = prover.sign(tx)

        logger.debug("Donate Tx created with id: " + signedTx.getId)
        val txId = ctx.sendTransaction(signedTx)
        logger.info("Donate Transaction Sent with TxId: " + txId)
        donateReqDAO.updateDonateTxId(req.id, signedTx.getId)
        donateReqDAO.updateStateById(req.id, 1)
      })
    } catch {
      case e:Throwable =>
        logger.error(utils.getStackTraceStr(e))
        throw new Throwable("Something is wrong on donating")
    }
  }

  def isReady(req: DonateReq): Boolean = {
    client.getClient.execute(ctx => {
      logger.debug("Request state : "+ req.state.toString)
      val paymentBoxList = ctx.getCoveringBoxesFor(Address.create(req.paymentAddress), req.fee)
      logger.debug(paymentBoxList.getCoveredAmount.toString +", "+ req.fee.toString)
      if(paymentBoxList.isCovered) {
        donateReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
      }
      else {
        val numberTxInMempool = explorer.getNumberTxInMempoolByAddress(req.paymentAddress)
        if (numberTxInMempool > 0){
          donateReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
        }
      }
      if (req.state == 0) {
        if(paymentBoxList.isCovered) {
          logger.info(s"Payment found for request ${req.id}")
          return true
        }
      }
      else {
        if(utils.checkTransaction(req.donateTxID.getOrElse("")) == 1){
          donateReqDAO.updateStateById(req.id, 2)
        }
      }
    })
    false
  }
}
