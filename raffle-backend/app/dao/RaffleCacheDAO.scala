package dao

import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.{Raffle, RaffleCache}

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}

trait RaffleCacheComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class RafflesTable(tag: Tag) extends Table[RaffleCache](tag, "RAFFLES") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def name = column[String]("NAME")
    def description = column[String]("DESCRIPTION")
    def goal = column[Long]("GOAL")
    def raised = column[Long]("RAISED")
    def deadlineHeight = column[Long]("DEADLINE_HEIGHT")
    def serviceFee = column[Int]("SERVICE_FEE")
    def charityPercent = column[Int]("CHARITY_PERCENT")
    def charityAdd = column[String]("CHARITY_ADD")
    def ticketPrice = column[Long]("TICKET_PRICE")
    def picLinks = column[String]("PIC_LINKS")
    def tickets = column[Long]("TICKETS")
    def participants = column[Long]("PARTICIPANTS")
    def redeemedTickets = column[Long]("REDEEM_TICKETS")

    def state = column[String]("STATE")
    def raffleToken = column[String]("RAFFLE_TOKEN")
    def timeStamp = column[String]("TIME_STAMP")
    def isUpdating = column[Boolean]("IS_UPDATING")
    def completed = column[Boolean]("COMPLETED")

    def * = (id, name, description, goal, raised, deadlineHeight, serviceFee,
      charityPercent, charityAdd, ticketPrice, picLinks, tickets, participants, redeemedTickets, state, raffleToken,
      timeStamp, isUpdating, completed) <> (RaffleCache.tupled, RaffleCache.unapply)
  }

}

@Singleton()
class RaffleCacheDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext)
  extends RaffleCacheComponent
    with HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val raffles = TableQuery[RafflesTable]

  /**
   * inserts a request into db
   *
   */
  def insert(name: String, description: String, goal: Long, raised: Long,
             deadlineHeight: Long, serviceFee: Int, charityPercent: Int, charityAdd: String,
             ticketPrice: Long, picLinks: String, tickets: Long, participants: Long, state: String,
             raffleToken: String, timeStamp: String): Future[Unit] ={
    val action = raffles += RaffleCache(1, name, description, goal, raised, deadlineHeight, serviceFee,
      charityPercent, charityAdd, ticketPrice, picLinks, tickets, participants, 0, state, raffleToken,
      timeStamp, isUpdating = false, completed = false)
    db.run(action).map(_ => ())
  }

  def insert(raffle: Raffle, participants: Long, state: String, timeStamp: String): Future[Unit] ={
    val action = raffles += RaffleCache(1, raffle.name, raffle.description, raffle.goal, raffle.raised,
      raffle.deadlineHeight, raffle.serviceFee, raffle.charityPercent, raffle.charityAddr, raffle.ticketPrice,
      raffle.picLinks, raffle.tickets, participants, 0, state, raffle.tokenId, timeStamp, isUpdating = false,
      completed = false)
    db.run(action).map(_ => ())
  }

  /**
   * all raffles
   * @return list of CreateReq
   */
  def all: Future[Seq[RaffleCache]] = db.run(raffles.result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): RaffleCache = Await.result(db.run(raffles.filter(req => req.id === id).result.head), Duration.Inf)

  def byTokenId(tokenId: String): RaffleCache = Await.result(db.run(raffles.filter(req => req.raffleToken === tokenId).result.head), Duration.Inf)
  /**
   * deletes by id
   *
   * @param id request id
   */
  def deleteById(id: Long): Future[Int] = db.run(raffles.filter(req => req.id === id).delete)

  def updateStateById(id: Long, state: String): Future[Int] = {
    val q = for { c <- raffles if c.id === id } yield c.state
    val updateAction = q.update(state)
    db.run(updateAction)
  }

  def updateRaised(id: Long, raised: Long, tickets: Long, participants: Long): Unit ={
    db.run(raffles.filter(_.id === id).map(ra => (ra.raised, ra.tickets, ra.participants))
      .update((raised, tickets, participants)))
  }

  def updateRedeemed(id: Long, redeemed: Long): Unit =
    db.run(raffles.filter(_.id === id).map(ra => ra.redeemedTickets).update(redeemed))

  def updatingStatus(): Int = Await.result(db.run((for {c <- raffles} yield c.isUpdating).update(true)), Duration.Inf)

  def acceptUpdating(id: Long): Int = {
    val q = for { c <- raffles if c.id === id } yield c.isUpdating
    val updateAction = q.update(false)
    Await.result(db.run(updateAction), Duration.Inf)
  }

  def selectAfterUpdating(): Seq[RaffleCache] =
    Await.result(db.run(raffles.filter(raffle => raffle.isUpdating === true && raffle.completed === false).result), Duration.Inf)

  def completeByTokenId(tokenId: String): Future[Int] =
    db.run((for {c <- raffles if c.raffleToken === tokenId} yield c.completed).update(true))

}

