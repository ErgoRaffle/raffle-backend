package Services

import akka.actor.{Actor, ActorLogging}
import play.api.Logger
import raffle.{FinalizeUtils, RaffleCacheUtils}

object JobsUtil {
  val create = "create"
  val donate = "donate"
  val finished = "finalize"
  val update = "update"
  val initialize = "initialize"
}

class Jobs(createReqHandler: CreateReqHandler, donateReqHandler: DonateReqHandler, finalizeUtils: FinalizeUtils,
           raffleCacheUtils: RaffleCacheUtils)
  extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)

  def receive = {
    case JobsUtil.create =>
      logger.info(s"Creation Request Thread started")
      createReqHandler.handleReqs()
      logger.info("Creation Request Thread finished working")

    case JobsUtil.donate =>
      logger.info(s"Donation Request Thread started")
      donateReqHandler.handleReqs()
      logger.info("Donation Request Thread finished working")

    case JobsUtil.finished =>
      logger.info(s"Finalize Raffles Thread started")
      finalizeUtils.ProcessFinishedRaffles()
      logger.info("Finalize Raffles Thread finished working")

    case JobsUtil.update =>
      logger.info("Cache Update Thread started")
      raffleCacheUtils.raffleSearch()
      logger.info("Cache Update Thread finished working")

    case JobsUtil.initialize =>
      logger.info("Initializing database")
      raffleCacheUtils.raffleInitialSearch()
      logger.info("Initialization finished")
  }

}
