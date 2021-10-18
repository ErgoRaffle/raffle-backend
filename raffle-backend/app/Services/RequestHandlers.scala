package Services
import helpers.{Configs, Utils, connectionException, failedTxException, skipException}
import javax.inject.Inject
import network.{Client, Explorer}
import play.api.Logger
import raffle.{CreateReqUtils, DonateReqUtils, FinalizeReqUtils, RaffleCacheUtils, RaffleUtils}
import dao.{CreateReqDAO, DonateReqDAO}
import models.{CreateReq, DonateReq}
import org.ergoplatform.appkit.{Address, ErgoClientException, InputBox}

import scala.collection.JavaConverters._
import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.util.Try

class CreateReqHandler@Inject ()(client: Client, createReqDAO: CreateReqDAO,
                                 utils: Utils, createReqUtils: CreateReqUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("Handling Creation requests...")

    createReqDAO.all.onComplete(requests => {
      var serviceBox : InputBox = null
      requests.get.map(req => {
        try{
          if (req.ttl <= utils.currentTime || req.state == 2) {
            handleRemoval(req)
          } else {
            serviceBox = handleReq(req, serviceBox)
          }
        } catch {
          case e: Throwable => logger.error(utils.getStackTraceStr(e))
        }
      })
    })

    try createReqUtils.independentMergeTxGeneration()
    catch {
      case _: org.ergoplatform.appkit.ErgoClientException =>
      case _: connectionException =>
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }

  def handleRemoval(req: CreateReq): Unit = try {
    val unSpentPayment = client.getAllUnspentBox(Address.create(req.paymentAddress))
    logger.info("Trying to remove request " + req.id)

    if (unSpentPayment.nonEmpty) {
      try {
        val unSpentPaymentBoxes = client.getCoveringBoxesFor(Address.create(req.paymentAddress), Configs.infBoxVal)
        if (unSpentPaymentBoxes.getCoveredAmount >= Configs.fee * 4) {
          logger.info(s"Request ${req.id} is going back to the request pool, creation fee is enough")
          createReqDAO.updateTTL(req.id, utils.currentTime + Configs.creationDelay)
          throw skipException()
        }
      } catch {
        case _: connectionException => throw new Throwable
        case _: failedTxException => throw new Throwable
        case e: skipException => throw e
        case _: Throwable => logger.error(s"Checking creation request ${req.id} failed")
      }
    }
    else {
      logger.info(s"will remove request: ${req.id} with state: ${req.state}")
      createReqDAO.deleteById(req.id)
    }
  } catch{
    case _: org.ergoplatform.appkit.ErgoClientException =>
    case e: Throwable => logger.error(utils.getStackTraceStr(e))
  }

  def handleReq(req: CreateReq, serviceBox: InputBox): InputBox = {
    try {
      var newServiceBox = serviceBox
      if (createReqUtils.isReady(req)) {
        logger.info(s"Creation Request ${req.id} is Ready, Executing the request with state: " + req.state)
        if(serviceBox == null) newServiceBox = utils.getServiceBox()
        newServiceBox = createReqUtils.generateAndSendBothTx(req, newServiceBox)
      }
      newServiceBox
    }
    catch {
      case _: connectionException => serviceBox
      case _: failedTxException => serviceBox
      case e: Throwable => {
        logger.error(s"Creation request ${req.id} failed")
        serviceBox
      }
    }
  }
}


class DonateReqHandler@Inject ()(client: Client, donateReqDAO: DonateReqDAO,
                                 utils: Utils, donateReqUtils: DonateReqUtils, explorer: Explorer, raffleUtils: RaffleUtils){
  private val logger: Logger = Logger(this.getClass)

  def handleReqs(): Unit = {
    logger.info("DonateReq Handling requests...")
    donateReqDAO.all.onComplete(requests => {
      var raffleMap : Map[String, InputBox] = Map();
      requests.get.map(req => {
        try{
          if (req.ttl <= utils.currentTime || req.state == 2) {
            handleRemoval(req)
          } else {
            raffleMap = handleReq(req, raffleMap)
          }
        } catch {
          case e: Throwable => logger.error(utils.getStackTraceStr(e))
        }
      })
    })
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
          case _: skipException =>
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

  def handleReq(req: DonateReq, raffleMap: Map[String, InputBox]): Map[String, InputBox] = {
    try {
      var outputMap = raffleMap
      if (donateReqUtils.isReady(req)) {
        if (client.getHeight >= req.raffleDeadline) {
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
          if (!outputMap.contains(req.raffleToken)) outputMap += (req.raffleToken -> utils.getRaffleBox(req.raffleToken))
          try {
            outputMap += (req.raffleToken -> donateReqUtils.createDonateTx(req, outputMap(req.raffleToken)))
            return outputMap
          }
          catch {
            case e: Throwable => logger.info(s"Donation failed for request ${req.id}")
          }
        }
      }
      raffleMap
    } catch {
      case _: connectionException => raffleMap
      case e: Throwable => {
        logger.warn(s"Error while processing donate request ${req.id}")
        logger.error(utils.getStackTraceStr(e))
        raffleMap
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
      case e: ErgoClientException => logger.warn(e.getMessage)
      case e: Throwable => logger.error(utils.getStackTraceStr(e))
    }
  }
}

