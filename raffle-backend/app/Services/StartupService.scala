package Services

import akka.actor.{ActorRef, ActorSystem, Props}
import network.Client
import play.api.Logger
import javax.inject.{Inject, Singleton}
import helpers.Configs

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@Singleton
class StartupService @Inject()(node: Client, system: ActorSystem, createReqHandler: CreateReqHandler,
                               donateReqHandler: DonateReqHandler, refundReqHandler: RefundReqHandler,
                               raffleUpdateHandler: RaffleUpdateHandler) (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()

  val jobs: ActorRef = system.actorOf(Props(new Jobs(createReqHandler, donateReqHandler,
    refundReqHandler, raffleUpdateHandler)), "scheduler")

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Configs.creationThreadInterval.seconds,
    receiver = jobs,
    message = JobsUtil.create
  )

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Configs.donateThreadInterval.seconds,
    receiver = jobs,
    message = JobsUtil.donate
  )

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Configs.updateThreadInterval.seconds,
    receiver = jobs,
    message = JobsUtil.update
  )

  if(Configs.activeFinalize) {
    system.scheduler.scheduleAtFixedRate(
      initialDelay = 2.seconds,
      interval = Configs.refundThreadInterval.seconds,
      receiver = jobs,
      message = JobsUtil.refund
    )
  }
}
