package Services

import java.util.Calendar

import helpers.{Configs, Utils}
import javax.inject.Inject
import network.Client
import play.api.Logger
import raffle.{CreateReqUtils, DonateReqUtils, RaffleUtils, RefundReqUtils}
import dao.{ActiveRafflesDAO, CreateReqDAO, DonateReqDAO, RefundReqDAO}
import models.{ActiveRaffle, CreateReq, DonateReq, RefundReq}

import scala.concurrent._
import ExecutionContext.Implicits.global

class CreateReqHandler@Inject ()(client: Client, createReqDAO: CreateReqDAO,
                                 utils: Utils, createReqUtils: CreateReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    println("Task is running ....")
    logger.info("Handling requests...")
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    createReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.ttl <= currentTime || req.state == 3) {
            handleRemoval(req)
          } else {
            println("Handling Creation Request with id: "+ req.id)
            println("Current Time: "+currentTime+", Request timeout: "+req.timeOut+", Request ttl: "+req.ttl)
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
      println("Request is Ready, Executing the request with state: "+ req2.state)
      //      updateServiceBox()
      if(!createReqUtils.isValid(req2)){
        createReqUtils.update(req2)
        req2 = createReqDAO.byId(req2.id)
        println("Request updated, with state: "+ req2.state)
      }
      createReqUtils.nextStage(req2)
    }
  }
}


class DonateReqHandler@Inject ()(client: Client, donateReqDAO: DonateReqDAO,
                                 utils: Utils, donateReqUtils: DonateReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    println("Donate handling is in process ....")
    logger.info("Handling requests...")
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    donateReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.ttl <= currentTime || req.state == 2) {
            handleRemoval(req)
          } else {
            println("Handling Donation Request with id: "+ req.id)
            println("Current Time: "+currentTime+", Request timeout: "+req.timeOut+", Request ttl: "+req.ttl)
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
    logger.info(s"will remove request: ${req.id} with state: ${req.state}")
    donateReqDAO.deleteById(req.id)
  }

  def handleReq(req: DonateReq): Unit = {
    var req2 = req
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    if(donateReqUtils.isReady(req) || req.timeOut <= currentTime){
      donateReqDAO.updateTimeOut(req.id, currentTime + Configs.checkingDelay)
      req2 = donateReqDAO.byId(req.id)
      println("Request is Ready, Executing the request with state: "+ req2.state)
      //      updateServiceBox()
      if(!donateReqUtils.isValid(req2)){
        donateReqUtils.update(req2)
        req2 = donateReqDAO.byId(req2.id)
        println("Request updated, with state: "+ req2.state)
      }
      donateReqUtils.nextStage(req2)
    }
  }
}


class RefundReqHandler@Inject ()(client: Client, refundReqDAO: RefundReqDAO,
                                 utils: Utils, refundReqUtils: RefundReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    println("Refund handling is in process ....")
    logger.info("Handling requests...")
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    refundReqDAO.all.map(reqs => {
      reqs.foreach(req => {
        try {
          if (req.state == 2) {
            handleRemoval(req)
          } else {
            println("Handling Donation Request with id: "+ req.id)
            println("Current Time: "+currentTime+", Request timeout: "+req.timeOut)
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

  def handleRemoval(req: RefundReq): Unit = {
    logger.info(s"will remove request: ${req.id} with state: ${req.state}")
    refundReqDAO.deleteById(req.id)
  }

  def handleReq(req: RefundReq): Unit = {
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    if(refundReqUtils.isReady(req) || req.timeOut <= currentTime){
      refundReqDAO.updateTimeOut(req.id, currentTime + Configs.checkingDelay)
      refundReqUtils.Refund(req)
    }
  }
}


class ActiveRaffleHandler@Inject ()(client: Client, activeRafflesDAO: ActiveRafflesDAO,
                                 utils: Utils, raffleUtils: RaffleUtils){
  private val logger: Logger = Logger(this.getClass)

  def handle(): Unit = {
    println("Active Raffle checking is in process ....")
    logger.info("Handling requests...")
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    activeRafflesDAO.all.map(raffles => {
      raffles.foreach(raffle => {
        try {
          if (raffle.state == 10) {
            handleRemoval(raffle)
          } else {
            handleReq(raffle)
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

  def handleRemoval(raffle: ActiveRaffle): Unit = {
    logger.info(s"will remove request: ${raffle.id} with state: ${raffle.state}")
    activeRafflesDAO.deleteById(raffle.id)
  }

  def handleReq(raffle: ActiveRaffle): Unit = {
    val currentTime = Calendar.getInstance().getTimeInMillis / 1000

    if(raffleUtils.isReady(raffle) || raffle.timeOut <= currentTime){
      val raffle2 = activeRafflesDAO.byId(raffle.id)
      activeRafflesDAO.updateTimeOut(raffle.id, currentTime + Configs.checkingDelay)
      raffleUtils.nextStage(raffle2)
    }
  }
}