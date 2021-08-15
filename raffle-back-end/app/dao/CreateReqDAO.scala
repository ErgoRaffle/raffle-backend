package dao

import helpers.Configs
import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.CreateReq

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait CreateReqComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class CreateReqTable(tag: Tag) extends Table[CreateReq](tag, "CREATE_REQUESTS") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("NAME")
    def description = column[String]("DESCRIPTION")
    def minToRaise = column[Long]("MIN_TO_RAISE")
    def deadlineHeight = column[Long]("DEADLINE_HEIGHT")
    def winnerPercent = column[Int]("WINNER_PERCENT")
    def charityAdd = column[String]("CHARITY_ADD")
    def ticketPrice = column[Long]("TICKET_PRICE")

    def state = column[Int]("STATE")
    def paymentAddress = column[String]("PAYMENT_ADD")
    def proxyAddress = column[String]("PROXY_ADD")
    def raffleAddress = column[String]("RAFFLE_ADD")
    def raffleToken = column[String]("RAFFLE_TOKEN")
    def serviceBoxId = column[String]("SERVICE_BOX_ID")
    def signedProxyTx = column[String]("S_PROXY_TX")
    def signedCreateTx = column[String]("S_CREATE_TX")

    def chainedWith = column[Long]("CHAINED_WITH")
    def isChained = column[Boolean]("IS_CHAINED")
    def timeOut = column[Long]("TIME_OUT")
    def ttl = column[Long]("TTL")

    def * = (id, name, description, minToRaise, deadlineHeight, winnerPercent, charityAdd,
      ticketPrice, state, paymentAddress, proxyAddress, raffleAddress, raffleToken, serviceBoxId, signedProxyTx.?,
      signedCreateTx.?, chainedWith, isChained, timeOut, ttl) <> (CreateReq.tupled, CreateReq.unapply)
  }

}

@Singleton()
class CreateReqDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext)
  extends CreateReqComponent
    with HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val requests = TableQuery[CreateReqTable]

  /**
   * inserts a request into db
   *
   */
  def insert(name: String, description: String, minToRaise: Long,
             deadlineHeight: Long, winnerPercent: Int, charityAdd: String,
             ticketPrice: Long, state: Int, paymentAddress: String,
             proxyAddress: String, raffleAddress: String, raffleToken: String,
             serviceBoxId: String, signedProxyTx: Option[String],  signedCreateTx: Option[String],
             chainedWith: Long, isChained: Boolean, timeOut: Long, ttl: Long): Future[Unit] ={
    val action = requests += CreateReq(1, name, description, minToRaise, deadlineHeight, winnerPercent, charityAdd,
      ticketPrice, state, paymentAddress, proxyAddress, raffleAddress, raffleToken, serviceBoxId, signedProxyTx,
      signedCreateTx, chainedWith, isChained, timeOut, ttl)
    db.run(action).map(_ => ())
  }

  /**
   * all requests
   * @return list of CreateReq
   */
  def all: Future[Seq[CreateReq]] = db.run(requests.result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): CreateReq = Await.result(db.run(requests.filter(req => req.id === id).result.head), Duration.Inf)

  /**
   * deletes by id
   *
   * @param id request id
   */
  def deleteById(id: Long): Future[Int] = db.run(requests.filter(req => req.id === id).delete)

  def updateStateById(id: Long, state: Int): Future[Int] = {
    val q = for { c <- requests if c.id === id } yield c.state
    val updateAction = q.update(state)
    db.run(updateAction)
  }

  def updateSignedProxyTx(id: Long, SPTx: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.signedProxyTx
    val updateAction = q.update(SPTx)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateSignedCreationTx(id: Long, SCTx: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.signedCreateTx
    val updateAction = q.update(SCTx)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateServiceBoxId(id: Long, SVBId: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.serviceBoxId
    val updateAction = q.update(SVBId)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateRaffleToken(id: Long, token: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.raffleToken
    val updateAction = q.update(token)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateTTL(id: Long, ttl: Long): Int = {
    val q = for { c <- requests if c.id === id } yield c.ttl
    val updateAction = q.update(ttl)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateTimeOut(id: Long, time: Long): Int = {
    val q = for { c <- requests if c.id === id } yield c.timeOut
    val updateAction = q.update(time)
    Await.result(db.run(updateAction), Duration.Inf)
  }
}

