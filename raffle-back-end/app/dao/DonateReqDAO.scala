package dao

import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.DonateReq

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait DonateReqComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class DonateReqTable(tag: Tag) extends Table[DonateReq](tag, "DONATE_REQUESTS") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def ticketCount = column[Long]("TICKET_COUNT")
    def ticketPrice = column[Long]("TICKET_PRICE")

    def state = column[Int]("STATE")
    def paymentAddress = column[String]("PAYMENT_ADD")
    def raffleAddress = column[String]("RAFFLE_ADD")
    def raffleToken = column[String]("RAFFLE_TOKEN")
    def signedDonateTx = column[String]("S_DONATE_TX")
    def participantAddress = column[String]("PARTICIPANT_ADD")
    def ticketAddress = column[String]("TICKET_ADD")

    def timeOut = column[Long]("TIME_OUT")
    def ttl = column[Long]("TTL")

    // TODO: Set default values
    def * = (id, ticketCount, ticketPrice, state, paymentAddress, raffleAddress,
      raffleToken, signedDonateTx.?, participantAddress, ticketAddress, timeOut, ttl) <> (DonateReq.tupled, DonateReq.unapply)
  }

}

@Singleton()
class DonateReqDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext)
  extends DonateReqComponent
    with HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val requests = TableQuery[DonateReqTable]

  /**
   * inserts a request into db
   *
   */
  def insert(ticketCount:Long, ticketPrice: Long, state: Int, paymentAddress: String,
             raffleAddress: String, raffleToken: String, signedDonateTx: Option[String],
             participantAddress: String, ticketAddress: String, timeOut: Long, ttl: Long): Unit ={
    val action = requests += DonateReq(1, ticketCount, ticketPrice, state, paymentAddress, raffleAddress, raffleToken,
      signedDonateTx, participantAddress, ticketAddress, timeOut, ttl)
    Await.result(db.run(action).map(_ => ()), Duration.Inf)
  }

  /**
   * all requests
   * @return list of Req
   */
  def all: Future[Seq[DonateReq]] = db.run(requests.result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): DonateReq = Await.result(db.run(requests.filter(req => req.id === id).result.head), Duration.Inf)

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

  def updateSignedDonateTx(id: Long, SDTx: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.signedDonateTx
    val updateAction = q.update(SDTx)
    Await.result(db.run(updateAction), Duration.Inf)
    // TODO: Replace Awaits with DB multi action
  }

  def updateTicketAddress(id: Long, ADD: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.ticketAddress
    val updateAction = q.update(ADD)
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

