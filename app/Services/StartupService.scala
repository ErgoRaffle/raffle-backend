package Services

import akka.actor.ActorSystem
import network.Client
import play.api.Logger
import play.api.inject.ApplicationLifecycle

import javax.inject.Inject
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartupService @Inject()(appLifecycle: ApplicationLifecycle, system: ActorSystem, node: Client)
                              (implicit ec: ExecutionContext) {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")
  node.setClient()


  appLifecycle.addStopHook { () =>
    logger.info("App stopped!")
    Future.successful(())
  }
}