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
      logger.info(s"Creation Request Thread is running")
      createReqHandler.handleReqs()

    case JobsUtil.donate =>
      logger.info(s"Donate Request Thread is running")
      donateReqHandler.handleReqs()

    case JobsUtil.refund =>
      logger.info(s"Refund Request Thread is running")
      refundReqHandler.handleReqs()
  }

}
