package Services

import akka.actor.{Actor, ActorLogging}
import play.api.Logger
import raffle.RaffleUtils

object JobsUtil {
  val create = "create"
  val donate = "donate"
  val refund = "refund"
  val raffle = "raffle"
  val search = "search"
}

class Jobs(createReqHandler: CreateReqHandler, donateReqHandler: DonateReqHandler, refundReqHandler: RefundReqHandler,
           raffleHandler: ActiveRaffleHandler, raffleUtils: RaffleUtils) extends Actor with ActorLogging {
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

    case JobsUtil.raffle =>
      raffleHandler.handle()
      logger.info(s"Active Raffle Checker Thread is running")

    case JobsUtil.search =>
      raffleUtils.raffleSearch()
      logger.info(s"Searching for new raffles")

  }


}
