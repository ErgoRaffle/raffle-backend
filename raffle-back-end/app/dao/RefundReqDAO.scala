package dao

import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.RefundReq

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait RefundReqComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class RefundReqTable(tag: Tag) extends Table[RefundReq](tag, "REFUND_REQUESTS") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def ticketCount = column[Long]("TICKET_COUNT")
    def ticketPrice = column[Long]("TICKET_PRICE")

    def state = column[Int]("STATE")
    def raffleAddress = column[String]("RAFFLE_ADD")
    def raffleToken = column[String]("RAFFLE_TOKEN")
    def signedRefundTx = column[String]("S_REFUND_TX")
    def ticketBoxId = column[String]("TICKET_ADD")

    def timeOut = column[Long]("TIME_OUT")

    // TODO: Set default values
    def * = (id, ticketCount, ticketPrice, state, raffleAddress,
      raffleToken, signedRefundTx.?, ticketBoxId, timeOut) <> (RefundReq.tupled, RefundReq.unapply)
  }

}

@Singleton()
class RefundReqDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext)
  extends RefundReqComponent
    with HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val requests = TableQuery[RefundReqTable]

  /**
   * inserts a request into db
   *
   */
  def insert(ticketCount:Long, ticketPrice: Long, state: Int,
             raffleAddress: String, raffleToken: String, signedRefundTx: Option[String],
             ticketBoxId: String, timeOut: Long): Unit ={
    val action = requests += RefundReq(1, ticketCount, ticketPrice, state,raffleAddress, raffleToken,
      signedRefundTx, ticketBoxId, timeOut)
    Await.result(db.run(action).map(_ => ()), Duration.Inf)
  }

  /**
   * all requests
   * @return list of Req
   */
  def all: Future[Seq[RefundReq]] = db.run(requests.result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): RefundReq = Await.result(db.run(requests.filter(req => req.id === id).result.head), Duration.Inf)

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

  def updateTimeOut(id: Long, time: Long): Future[Int] = {
    val q = for { c <- requests if c.id === id } yield c.timeOut
    val updateAction = q.update(time)
    db.run(updateAction)
  }
}

