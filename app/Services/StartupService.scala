package Services

import akka.actor.{ActorRef, ActorSystem, Props}
import network.Client
import play.api.Logger

import javax.inject.{Inject, Singleton}
import helpers.Configs
import raffle.{FinalizeUtils, RaffleCacheUtils}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration._

@Singleton
class StartupService @Inject()(node: Client, system: ActorSystem, createReqHandler: CreateReqHandler,
                               donateReqHandler: DonateReqHandler, finalizeUtils: FinalizeUtils,
                               raffleCacheUtils: RaffleCacheUtils)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()

  val jobs: ActorRef = system.actorOf(Props(new Jobs(createReqHandler, donateReqHandler,
    finalizeUtils, raffleCacheUtils)), "scheduler")

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
    initialDelay = 20.seconds,
    interval = Configs.updateThreadInterval.seconds,
    receiver = jobs,
    message = JobsUtil.update
  )

  system.scheduler.scheduleOnce(
    delay = 1.seconds,
    receiver = jobs,
    message = JobsUtil.initialize
  )

  if(Configs.activeFinalize) {
    system.scheduler.scheduleAtFixedRate(
      initialDelay = 2.seconds,
      interval = Configs.refundThreadInterval.seconds,
      receiver = jobs,
      message = JobsUtil.finished
    )
  }
}
