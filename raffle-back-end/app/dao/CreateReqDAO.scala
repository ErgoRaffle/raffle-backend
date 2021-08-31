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
    def goal = column[Long]("GOAL")
    def deadlineHeight = column[Long]("DEADLINE_HEIGHT")
    def charityPercent = column[Int]("CHARITY_PERCENT")
    def charityAddr = column[String]("CHARITY_ADDR")
    def ticketPrice = column[Long]("TICKET_PRICE")

    def state = column[Int]("STATE")
    def walletAddress = column[String]("WALLET_ADDR")
    def paymentAddress = column[String]("PAYMENT_ADDR")
    def createTxId = column[String]("CREATE_TX_ID")
    def mergeTxId = column[String]("MERGE_TX_ID")

    def timeStamp = column[String]("TIME_STAMP")
    def ttl = column[Long]("TTL")
    def deleted = column[Boolean]("DELETED")

    def * = (id, name, description, goal, deadlineHeight, charityPercent, charityAddr,
      ticketPrice, state, walletAddress, paymentAddress,
      createTxId.?, mergeTxId.?, timeStamp, ttl, deleted) <> (CreateReq.tupled, CreateReq.unapply)
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
  def insert(name: String, description: String, goal: Long, deadlineHeight: Long, charityPercent: Int,
             charityAddr: String, ticketPrice: Long, state: Int, walletAddress: String, paymentAddress: String,
             createTxId: Option[String],  mergeTxId: Option[String], timeStamp: String, ttl: Long): Future[Unit] ={
    val action = requests += CreateReq(1, name, description, goal, deadlineHeight, charityPercent, charityAddr,
      ticketPrice, state, walletAddress, paymentAddress, createTxId, mergeTxId, timeStamp, ttl, false)
    db.run(action.asTry).map( _ => ())
  }

  /**
   * all requests
   * @return list of CreateReq
   */
  def all: Future[Seq[CreateReq]] = db.run(requests.filter(_.deleted === false).result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): CreateReq = Await.result(db.run(requests.filter(_.deleted === false).filter(req => req.id === id).result.head), Duration.Inf)

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

  def updateCreateTxID(id: Long, TxId: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.createTxId
    val updateAction = q.update(TxId)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateMergeTxId(id: Long, TxId: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.mergeTxId
    val updateAction = q.update(TxId)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateTTL(id: Long, ttl: Long): Int = {
    val q = for { c <- requests if c.id === id } yield c.ttl
    val updateAction = q.update(ttl)
    Await.result(db.run(updateAction), Duration.Inf)
  }

}

