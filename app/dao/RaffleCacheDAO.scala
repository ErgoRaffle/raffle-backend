package dao

import javax.inject.{Inject, Singleton}
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile
import models.{Raffle, RaffleCache}
import raffle.raffleStatus
import raffle.raffleStatus._

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

    def state = column[Int]("STATE")
    def raffleToken = column[String]("RAFFLE_TOKEN", O.Unique)
    def creationTime = column[Long]("CREATION_TIME")
    def lastActivity = column[Long]("LAST_ACTIVITY")
    def isUpdating = column[Boolean]("IS_UPDATING")
    def completed = column[Boolean]("COMPLETED")

    def * = (id, name, description, goal, raised, deadlineHeight, serviceFee,
      charityPercent, charityAdd, ticketPrice, picLinks, tickets, participants, redeemedTickets, state, raffleToken,
      creationTime, lastActivity, isUpdating, completed) <> (RaffleCache.tupled, RaffleCache.unapply)
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
             ticketPrice: Long, picLinks: String, tickets: Long, participants: Long, state: Int,
             raffleToken: String, creationTime: Long, lastActivity: Long): Future[Unit] ={
    val action = raffles += RaffleCache(1, name, description, goal, raised, deadlineHeight, serviceFee,
      charityPercent, charityAdd, ticketPrice, picLinks, tickets, participants, redeemedTickets = 0, state, raffleToken,
      creationTime, lastActivity, isUpdating = false, completed = false)
    db.run(action).map(_ => ())
  }

  def insert(raffle: Raffle, participants: Long, state: Int, creationTime: Long, lastActivity: Long): Future[Unit] ={
    val action = raffles += RaffleCache(1, raffle.name, raffle.description, raffle.goal, raffle.raised,
      raffle.deadlineHeight, raffle.serviceFee, raffle.charityPercent, raffle.charityAddr, raffle.ticketPrice,
      raffle.picLinks, raffle.tickets, participants, redeemedTickets = 0, state, raffle.tokenId, creationTime, lastActivity,
      isUpdating = false, completed = false)
    db.run(action).map(_ => ())
  }

  def initialInsert(raffle: Raffle, participants: Long, creationTime: Long, lastActivity: Long,
                    maxRaised: Long, maxTicket: Long): Future[Unit] ={
    val action = raffles += RaffleCache(1, raffle.name, raffle.description, raffle.goal, maxRaised,
      raffle.deadlineHeight, raffle.serviceFee, raffle.charityPercent, raffle.charityAddr, raffle.ticketPrice,
      raffle.picLinks, maxTicket, participants, redeemedTickets = 0, raffleStatus.unknown.id, raffle.tokenId, creationTime, lastActivity,
      isUpdating = false, completed = false)
    db.run(action).map(_ => ())
  }

  /**
   * all raffles
   * @return list of CreateReq
   */
  def all: Seq[RaffleCache] = Await.result(db.run(raffles.result), Duration.Inf)

   /**
   * select raffles according to limit offset and status
   * @return list of CreateReq and total RaffleCaches
   */
  def selectRaffles(state: String, sorting: String, offset: Int, limit: Int): (Seq[RaffleCache], Int) = {
    val statesInsteadAll = List(active.id, succeed.id, failed.id)
    val q = for {
      querySelectRaffles <- DBIO.successful(if (state == "all") raffles.filter(_.state inSet statesInsteadAll) else raffles.filter(_.state === raffleStatus.withName(state).id))
      setLimitOffset <- {
        val  setLimitOffset = {
          sorting.toLowerCase match {
            case "createtime" => querySelectRaffles.sortBy(_.creationTime.desc)
            case "-createtime" => querySelectRaffles.sortBy(_.creationTime.asc)
            case "deadline" => querySelectRaffles.sortBy(_.deadlineHeight.desc)
            case "-deadline" => querySelectRaffles.sortBy(_.deadlineHeight.asc)
            case "activity" => querySelectRaffles.sortBy(_.lastActivity.desc)
            case "-activity" => querySelectRaffles.sortBy(_.lastActivity.asc)
            case _ => querySelectRaffles.sortBy(_.lastActivity.desc)
          }
        }
        val selectQ = setLimitOffset.drop(offset).take(limit)
        selectQ.result
      }
      total <- querySelectRaffles.length.result
    } yield (setLimitOffset, total)

    Await.result(db.run(q), Duration.Inf)
  }

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
  def deleteByTokenId(id: String): Future[Int] = db.run(raffles.filter(req => req.raffleToken === id).delete)

  def updateStateById(id: Long, state: Int): Future[Int] = {
    val q = for { c <- raffles if c.id === id } yield c.state
    val updateAction = q.update(state)
    db.run(updateAction)
  }

  def updateRaised(id: Long, raised: Long, tickets: Long): Unit =
    db.run(raffles.filter(_.id === id).map(ra => (ra.raised, ra.tickets)).update((raised, tickets)))

  def updateActivity(id: Long, raised: Long, tickets: Long, participants: Long, lastActivity: Long): Unit =
    db.run(raffles.filter(_.id === id).map(ra => (ra.raised, ra.tickets, ra.participants, ra.lastActivity))
      .update((raised, tickets, participants, lastActivity)))

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

