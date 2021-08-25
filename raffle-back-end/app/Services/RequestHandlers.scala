package Services

import java.util.Calendar

import helpers.{Configs, Utils}
import javax.inject.Inject
import network.{Client, Explorer}
import play.api.Logger
import raffle.{CreateReqUtils, DonateReqUtils, FinalizeReqUtils, RaffleUtils}
import dao.{CreateReqDAO, DonateReqDAO}
import models.{CreateReq, DonateReq}
import org.ergoplatform.appkit.Address

import scala.concurrent._
import ExecutionContext.Implicits.global

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
    catch {case e: Throwable => logger.error(utils.getStackTraceStr(e))}
  }

  def handleRemoval(req: CreateReq): Unit = {
    logger.info(s"will remove request: ${req.id} with state: ${req.state}")
    createReqDAO.deleteById(req.id)
  }

  def handleReq(req: CreateReq): Unit = {
    var req2 = req

    if(createReqUtils.isReady(req) || req.timeOut <= utils.currentTime){
      createReqDAO.updateTimeOut(req.id, utils.currentTime + Configs.checkingDelay)
      req2 = createReqDAO.byId(req.id)
      logger.debug("Request is Ready, Executing the request with state: "+ req2.state)
      createReqUtils.nextStage(req2)
    }
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
            logger.info("Handling Donation Request with id: "+ req.id)
            logger.info("Current Time: "+utils.currentTime+", Request timeout: "+req.timeOut+", Request ttl: "+req.ttl)
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
    val unSpentPaymentBoxes = client.getAllUnspentBox(Address.create(req.paymentAddress))
    val numberTxInMempool = explorer.getNumberTxInMempoolByAddress(req.paymentAddress)

    if (unSpentPaymentBoxes.nonEmpty && numberTxInMempool == 0) {
      raffleUtils.refundBoxes(unSpentPaymentBoxes, Address.create(req.participantAddress))
      donateReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
    }
    else if (numberTxInMempool > 0){
      // TODO: Add ChainTx for refund payments in mempool
      donateReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
    }
    else {
      logger.info(s"will remove donate request: ${req.id} with state: ${req.state}")
      donateReqDAO.deleteById(req.id)
    }
  }

  def handleReq(req: DonateReq): Unit = {
    if (donateReqUtils.isReady(req) || req.timeOut <= utils.currentTime) {
      if (client.getHeight> req.raffleDeadline) {
        logger.info("Start refund process for "+ req.id)
        val unSpentPaymentBoxes = client.getAllUnspentBox(Address.create(req.paymentAddress))
        val txId = raffleUtils.refundBoxes(unSpentPaymentBoxes, Address.create(req.participantAddress))
        donateReqDAO.updateReq(req.id, 1, txId, utils.currentTime + Configs.creationDelay)
        logger.info(s"Refund process done, for: ${req.id} with txId: ${txId}" )
      }
      else {
        logger.info("Donate Request is Ready, Executing the request with state: "+ req.state)
        donateReqUtils.createDonateTx(req)
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
