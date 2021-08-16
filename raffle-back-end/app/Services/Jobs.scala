package Services

import akka.actor.{Actor, ActorLogging}
import play.api.Logger

object JobsUtil {
  val create = "create"
  val donate = "donate"
  val refund = "refund"
  val raffle = "raffle"
}

class Jobs(createReqHandler: CreateReqHandler, donateReqHandler: DonateReqHandler, refundReqHandler: RefundReqHandler)
  extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)

  def receive = {
    case JobsUtil.create =>
      createReqHandler.handleReqs()
      logger.info(s"Creation Request Thread is running")

    case JobsUtil.donate =>
      donateReqHandler.handleReqs()
      logger.info(s"Donate Request Thread is running")

    case JobsUtil.refund =>
      refundReqHandler.handleReqs()
      logger.info(s"Refund Request Thread is running")
  }

}
