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
    def raffleDeadline = column[Long]("RAFFLE_DEADLINE")
    def state = column[Int]("STATE")
    def paymentAddress = column[String]("PAYMENT_ADD")
    def raffleToken = column[String]("RAFFLE_TOKEN")
    def donateTxId = column[String]("DONATE_TX_ID")
    def participantAddress = column[String]("PARTICIPANT_ADD")

    def timeStamp = column[String]("TIME_STAMP")
    def ttl = column[Long]("TTL")
    def deleted = column[Boolean]("DELETED")

    // TODO: Set default values
    def * = (id, ticketCount, ticketPrice, raffleDeadline, state, paymentAddress,
      raffleToken, donateTxId.?, participantAddress, timeStamp, ttl, deleted) <> (DonateReq.tupled, DonateReq.unapply)
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
  def insert(ticketCount:Long, ticketPrice: Long, raffleDeadline: Long , state: Int, paymentAddress: String,
             raffleToken: String, signedDonateTx: Option[String], participantAddress: String, timeStamp: String, ttl: Long): Unit ={
    val action = requests += DonateReq(1, ticketCount, ticketPrice, raffleDeadline, state, paymentAddress, raffleToken,
      signedDonateTx, participantAddress, timeStamp, ttl, false)
    Await.result(db.run(action).map(_ => ()), Duration.Inf)
  }

  /**
   * all requests
   * @return list of Req
   */
  def all: Future[Seq[DonateReq]] = db.run(requests.filter(_.deleted === false).result)

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
  def deleteById(id: Long): Future[Int] = db.run(requests.filter(req => req.id === id).map(req => req.deleted).update(true))

  def updateStateById(id: Long, state: Int): Future[Int] = {
    val q = for { c <- requests if c.id === id } yield c.state
    val updateAction = q.update(state)
    db.run(updateAction)
  }

  def updateReq(id: Long, state: Int, txId: String, ttl: Long): Future[Int] = {
    db.run(requests.filter(_.id === id).map(req => (req.state, req.donateTxId, req.ttl)).update((state, txId, ttl)))
  }

  def updateDonateTxId(id: Long, SDTx: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.donateTxId
    val updateAction = q.update(SDTx)
    Await.result(db.run(updateAction), Duration.Inf)
    // TODO: Replace Awaits with DB multi action
  }

  def updateTTL(id: Long, ttl: Long): Int = {
    val q = for { c <- requests if c.id === id } yield c.ttl
    val updateAction = q.update(ttl)
    Await.result(db.run(updateAction), Duration.Inf)
  }

}

