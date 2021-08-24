package Services

import java.util.Calendar

import helpers.{Configs, Utils}
import javax.inject.Inject
import network.Client
import play.api.Logger
import raffle.{CreateReqUtils, DonateReqUtils, FinalizeReqUtils}
import dao.{CreateReqDAO, DonateReqDAO}
import models.{CreateReq, DonateReq}

import scala.concurrent._
import ExecutionContext.Implicits.global

class CreateReqHandler@Inject ()(client: Client, createReqDAO: CreateReqDAO,
                                 utils: Utils, createReqUtils: CreateReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("Handling Creation requests...")
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    createReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.ttl <= currentTime || req.state == 3) {
            handleRemoval(req)
          } else {
            logger.debug("Handling Creation Request with id: "+ req.id)
            logger.debug("Current Time: "+currentTime+", Request timeout: "+req.timeOut+", Request ttl: "+req.ttl)
            handleReq(req)
          }
        } catch {
          case e: Throwable => e.printStackTrace()
            logger.error(e.getMessage)
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
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    if(createReqUtils.isReady(req) || req.timeOut <= currentTime){
      createReqDAO.updateTimeOut(req.id, currentTime + Configs.checkingDelay)
      req2 = createReqDAO.byId(req.id)
      logger.debug("Request is Ready, Executing the request with state: "+ req2.state)
      createReqUtils.nextStage(req2)
    }
  }
}


class DonateReqHandler@Inject ()(client: Client, donateReqDAO: DonateReqDAO,
                                 utils: Utils, donateReqUtils: DonateReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("DonateReq Handling requests...")
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    donateReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.ttl <= currentTime || req.state == 2) {
            handleRemoval(req)
          } else {
            logger.info("Handling Donation Request with id: "+ req.id)
            logger.info("Current Time: "+currentTime+", Request timeout: "+req.timeOut+", Request ttl: "+req.ttl)
            handleReq(req)
          }
        } catch {
          case e: Throwable => e.printStackTrace()
            logger.error(e.getMessage)
        }
      })
    }) recover {
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def handleRemoval(req: DonateReq): Unit = {
    logger.info(s"will remove donate request: ${req.id} with state: ${req.state}")
    donateReqDAO.deleteById(req.id)
  }

  def handleReq(req: DonateReq): Unit = {
    var req2 = req
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    if(donateReqUtils.isReady(req) || req.timeOut <= currentTime){
      donateReqDAO.updateTimeOut(req.id, currentTime + Configs.checkingDelay)
      req2 = donateReqDAO.byId(req.id)
      logger.info("Donate Request is Ready, Executing the request with state: "+ req2.state)
      donateReqUtils.createDonateTx(req2)
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
