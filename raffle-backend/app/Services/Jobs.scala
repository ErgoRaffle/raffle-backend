package Services

import akka.actor.{Actor, ActorLogging}
import play.api.Logger
import raffle.RaffleCacheUtils

object JobsUtil {
  val create = "create"
  val donate = "donate"
  val refund = "refund"
  val update = "update"
  val initialize = "initialize"
}

class Jobs(createReqHandler: CreateReqHandler, donateReqHandler: DonateReqHandler, refundReqHandler: RefundReqHandler,
           raffleCacheUtils: RaffleCacheUtils)
  extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)

  def receive = {
    case JobsUtil.create =>
      logger.info(s"Creation Request Thread is running")
      createReqHandler.handleReqs()

    case JobsUtil.donate =>
      logger.info(s"Donate Request Thread is running")
      donateReqHandler.handleReqs()

    case JobsUtil.refund =>
      logger.info(s"Refund Request Thread is running")
      refundReqHandler.handleReqs()

    case JobsUtil.update =>
      logger.info("updating raffles information")
      raffleCacheUtils.raffleSearch()

    case JobsUtil.initialize =>
      logger.info("Initializing database")
      raffleCacheUtils.raffleInitialSearch()
  }

}
