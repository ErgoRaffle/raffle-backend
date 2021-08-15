package dao

import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.ActiveRaffle

import scala.concurrent
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait ActiveRafflesComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class ActiveRafflesTable(tag: Tag) extends Table[ActiveRaffle](tag, "ACTIVE_RAFFLES") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def minToRaise = column[Long]("MIN_TO_RAISE")
    def deadlineHeight = column[Long]("DEADLINE_HEIGHT")
    def winnerPercent = column[Int]("WINNER_PERCENT")
    def charityAdd = column[String]("CHARITY_ADD")
    def ticketPrice = column[Long]("TICKET_PRICE")

    def state = column[Int]("STATE")
    def raffleAddress = column[String]("RAFFLE_ADD")
    def raffleToken = column[String]("RAFFLE_TOKEN")
    def winnerBoxId = column[String]("WINNER_BOX_ID")

    def timeOut = column[Long]("TIME_OUT")
    def isUpdating = column[Boolean]("IS_UPDATING")

    def * = (id, minToRaise, deadlineHeight, winnerPercent, charityAdd,
      ticketPrice, state, raffleAddress, raffleToken, winnerBoxId, timeOut, isUpdating) <> (ActiveRaffle.tupled, ActiveRaffle.unapply)
  }

}

@Singleton()
class ActiveRafflesDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext)
  extends ActiveRafflesComponent
    with HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val requests = TableQuery[ActiveRafflesTable]

  /**
   * inserts a request into db
   *
   */
  def insert(minToRaise: Long, deadlineHeight: Long, winnerPercent: Int, charityAdd: String,
             ticketPrice: Long, state: Int, raffleAddress: String, raffleToken: String,
             winnerBoxId: String, timeOut: Long): Future[Unit] ={
    val action = requests += ActiveRaffle(1, minToRaise, deadlineHeight, winnerPercent, charityAdd,
      ticketPrice, state, raffleAddress, raffleToken, winnerBoxId, timeOut, isUpdating = false)
    db.run(action).map(_ => ())
  }

  /**
   * all requests
   * @return list of CreateReq
   */
  def all: Future[Seq[ActiveRaffle]] = db.run(requests.result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): ActiveRaffle = Await.result(db.run(requests.filter(req => req.id === id).result.head), Duration.Inf)

  def byTokenId(tokenId: String): ActiveRaffle = Await.result(db.run(requests.filter(req => req.raffleToken === tokenId).result.head), Duration.Inf)
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

  def updateTimeOut(id: Long, time: Long): Int = {
    val q = for { c <- requests if c.id === id } yield c.timeOut
    val updateAction = q.update(time)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updateWinnerBoxId(id: Long, boxId: String): Int = {
    val q = for { c <- requests if c.id === id } yield c.winnerBoxId
    val updateAction = q.update(boxId)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def updatingStatus(): Int = Await.result(db.run((for {c <- requests} yield c.isUpdating).update(true)), Duration.Inf)

  def acceptUpdating(id: Long): Int = {
    val q = for { c <- requests if c.id === id } yield c.isUpdating
    val updateAction = q.update(false)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def deleteAfterUpdating(): Future[Int] = db.run(requests.filter(raffle => raffle.isUpdating === true).delete)
}

