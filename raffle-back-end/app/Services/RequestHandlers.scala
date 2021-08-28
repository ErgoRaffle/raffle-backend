package Services
import helpers.{Configs, Utils, connectionException, failedTxException, skipException}
import javax.inject.Inject
import network.{Client, Explorer}
import play.api.Logger
import raffle.{CreateReqUtils, DonateReqUtils, FinalizeReqUtils, RaffleUtils}
import dao.{CreateReqDAO, DonateReqDAO}
import models.{CreateReq, DonateReq}
import org.ergoplatform.appkit.Address

import scala.collection.JavaConverters._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Try

class CreateReqHandler@Inject ()(client: Client, createReqDAO: CreateReqDAO,
                                 utils: Utils, createReqUtils: CreateReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("Handling Creation requests...")

    createReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.ttl <= utils.currentTime || req.state == 3) {
            handleRemoval(req)
          } else {
            logger.debug("Handling Creation Request with id: "+ req.id)
            logger.debug("Current Time: "+utils.currentTime+", Request timeout: "+req.timeOut+", Request ttl: "+req.ttl)
            handleReq(req)
          }
        } catch {
          case e: Throwable => logger.error(utils.getStackTraceStr(e))
        }
      })
    }) recover {
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
    try createReqUtils.independentMergeTxGeneration()
    catch {
      case _:connectionException =>
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def handleRemoval(req: CreateReq): Unit = {
    logger.info(s"will remove request: ${req.id} with state: ${req.state}")
    createReqDAO.deleteById(req.id)
  }

  def handleReq(req: CreateReq): Unit = {
    try {
      if (createReqUtils.isReady(req) || req.timeOut <= utils.currentTime) {
        logger.debug(s"Creation Request ${req.id} is Ready, Executing the request with state: " + req.state)
        createReqUtils.nextStage(req)
        createReqDAO.updateTimeOut(req.id, utils.currentTime + Configs.checkingDelay)
      }
    }
    catch {
      case _: connectionException =>
      case _: failedTxException =>
      case e: Throwable => logger.error(s"Creation request ${req.id} failed")}
  }
}


class DonateReqHandler@Inject ()(client: Client, donateReqDAO: DonateReqDAO,
                                 utils: Utils, donateReqUtils: DonateReqUtils, explorer: Explorer, raffleUtils: RaffleUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("DonateReq Handling requests...")

    donateReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.ttl <= utils.currentTime || req.state == 2) {
            handleRemoval(req)
          } else {
            handleReq(req)
          }
        } catch {
          case e: Throwable => logger.error(utils.getStackTraceStr(e))
        }
      })
    }) recover {
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def handleRemoval(req: DonateReq): Unit = {
    try {
      val unSpentPaymentBoxes = client.getAllUnspentBox(Address.create(req.paymentAddress))
      val numberTxInMempool = explorer.getNumberTxInMempoolByAddress(req.paymentAddress)
      logger.info("Trying to remove request " + req.id)

      if (unSpentPaymentBoxes.nonEmpty && numberTxInMempool == 0) {
        try {
          val unSpentPaymentBoxes = client.getCoveringBoxesFor(Address.create(req.paymentAddress), Configs.infBoxVal)
          if (unSpentPaymentBoxes.getCoveredAmount >= req.fee) {
            logger.info(s"Request ${req.id} is going back to the request pool, donation amount is enough")
            donateReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
            throw skipException()
          }
          logger.info(s"Request ${req.id} donation is not enough (${unSpentPaymentBoxes.getCoveredAmount} < ${req.fee}) start refunding process for " + req.id)
          val txId = raffleUtils.refundBoxes(unSpentPaymentBoxes.getBoxes.asScala.toList, Address.create(req.participantAddress))
          donateReqDAO.updateReq(req.id, 1, txId, utils.currentTime + Configs.creationDelay)
          logger.info(s"Refund process done, for: ${req.id} with txId: ${txId}")
        } catch {
          case _: connectionException => throw new Throwable
          case _: failedTxException => throw new Throwable
          case e: skipException => throw e
          case _: Throwable => logger.error(s"Failed donation refund for Request ${req.id} from ${req.participantAddress} with Donate Tx ${req.donateTxID} to the raffle ${req.raffleToken} failed refunding or checking")
        }
      }
      else if (numberTxInMempool > 0) {
        // TODO: Add ChainTx for refund payments in mempool
        donateReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
      }
      else {
        logger.info(s"removing donate request: ${req.id} with state: ${req.state}")
        donateReqDAO.deleteById(req.id)
      }
    } catch {
      case _: skipException =>
      case e: Throwable => logger.info(s"Removing failed for request ${req.id}")
    }
  }

  def handleReq(req: DonateReq): Unit = {
    try {
      if (donateReqUtils.isReady(req) || req.timeOut <= utils.currentTime) {
        donateReqDAO.updateTimeOut(req.id, utils.currentTime + Configs.checkingDelay)
        var inMempool = false
        Try {
          if (utils.checkTransaction(req.donateTxID.get) == 2) {
            logger.info(s"Donation Tx for request ${req.id} is already in Mempool skipping the process")
            inMempool = true
          }
        }
        if (!inMempool) {
          if (client.getHeight > req.raffleDeadline) {
            try {
              logger.info("Raffle deadline passed refunding request " + req.id)
              val unSpentPaymentBoxes = client.getAllUnspentBox(Address.create(req.paymentAddress))
              val txId = raffleUtils.refundBoxes(unSpentPaymentBoxes, Address.create(req.participantAddress))
              donateReqDAO.updateReq(req.id, 1, txId, utils.currentTime + Configs.creationDelay)
              logger.info(s"Refund process done, for: ${req.id} with txId: ${txId}")
            } catch {
              case e: Throwable => logger.info(s"Failed donation refund after deadline for Request ${req.id} to the raffle ${req.raffleToken}")
            }
          }
          else {
            logger.info(s"Donation Request ${req.id} is Ready, Executing the request with state: " + req.state)
            try donateReqUtils.createDonateTx(req)
            catch {
              case e: Throwable => logger.info(s"Donation failed for request ${req.id}")
            }
          }
        }
      }
    } catch {
      case _: connectionException =>
      case e: Throwable => {
        logger.warn(s"Error while processing donate request ${req.id}")
        logger.error(utils.getStackTraceStr(e))
      }
    }
  }
}


class RefundReqHandler@Inject ()(client: Client, utils: Utils, refundReqUtils: FinalizeReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("Handling finalize process...")
    try {
      refundReqUtils.Refund()
    } catch {
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }
}
