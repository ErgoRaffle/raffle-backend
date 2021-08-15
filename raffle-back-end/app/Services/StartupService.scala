package Services

import akka.actor.{ActorRef, ActorSystem, Props}
import network.Client
import play.api.Logger
import javax.inject.{Inject, Singleton}
import helpers.Configs
import raffle.RaffleUtils

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@Singleton
class StartupService @Inject()(node: Client, system: ActorSystem, createReqHandler: CreateReqHandler,
                               donateReqHandler: DonateReqHandler, refundReqHandler: RefundReqHandler,
                               raffleHandler: ActiveRaffleHandler, raffleUtils: RaffleUtils)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()

  val jobs: ActorRef = system.actorOf(Props(new Jobs(createReqHandler, donateReqHandler,
    refundReqHandler, raffleHandler, raffleUtils)), "scheduler")

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Configs.creationThreadInterval.seconds,
    receiver = jobs,
    message = JobsUtil.create
  )

//  system.scheduler.scheduleAtFixedRate(
//    initialDelay = 2.seconds,
//    interval = Configs.donateThreadInterval.seconds,
//    receiver = jobs,
//    message = JobsUtil.donate
//  )
//
//  system.scheduler.scheduleAtFixedRate(
//    initialDelay = 2.seconds,
//    interval = Configs.refundThreadInterval.seconds,
//    receiver = jobs,
//    message = JobsUtil.refund
//  )

//  system.scheduler.scheduleAtFixedRate(
//    initialDelay = 3.seconds,
//    interval = Configs.raffleThreadInterval.seconds,
//    receiver = jobs,
//    message = JobsUtil.raffle
//  )

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 10.seconds,
    interval = Configs.searchThreadInterval.seconds,
    receiver = jobs,
    message = JobsUtil.search
  )

}