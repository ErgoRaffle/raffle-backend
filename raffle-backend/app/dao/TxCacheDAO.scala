package dao

import javax.inject.{Inject, Singleton}
import models.TxCache
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration
import raffle.txType._

trait TxCacheComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class TransactionsTable(tag: Tag) extends Table[TxCache](tag, "TRANSACTIONS") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def txId = column[String]("TX_ID")
    def tokenId = column[String]("TOKEN_ID")
    def tokenCount = column[Long]("TOKEN_COUNT")
    def txType = column[Int]("TYPE")
    def walletAdd = column[String]("WALLET_ADD")
    def txToken = index("TX_TOKEN", (txId, tokenId), unique = true)
    def * = (id, txId, tokenId, tokenCount, txType, walletAdd) <> (TxCache.tupled, TxCache.unapply)
  }

}

@Singleton()
class TxCacheDAO @Inject()(protected val dbConfigProvider: DatabaseConfigProvider)(implicit executionContext: ExecutionContext)
  extends TxCacheComponent
    with HasDatabaseConfigProvider[JdbcProfile] {

  import profile.api._

  val Txs = TableQuery[TransactionsTable]

  /**
   * inserts a request into db
   *
   */
  def insert(txId: String, tokenId: String, tokenCount: Long, txType: Int,
             wallerAdd: String): Future[Unit] ={
    val action = Txs += TxCache(1, txId, tokenId, tokenCount, txType, wallerAdd)
    db.run(action).map(_ => ())
  }

  /**
   * all Transactions
   * @return list of CreateReq
   */
  def all: Future[Seq[TxCache]] = db.run(Txs.result)

  /**
   * @param id request id
   * @return request associated with the id
   */
  def byId(id: Long): TxCache = Await.result(db.run(Txs.filter(tx => tx.id === id).result.head), Duration.Inf)

  def byTokenId(tokenId: String, offset: Int, limit: Int): (Seq[TxCache], Int) = {
    val types = Seq(winner.id, charity.id)
    val q = for {
      successTxsQ <- DBIO.successful(Txs.filter(tx => tx.tokenId === tokenId && (tx.txType inSet types)))
      lengthTotalSuccessResult <- successTxsQ.length.result
      allTxQuery <- DBIO.successful(if (lengthTotalSuccessResult != 0) Txs.filter(tx => tx.tokenId === tokenId).sortBy(_.txType.asc)
                                    else Txs.filter(tx => tx.tokenId === tokenId && tx.txType === refund.id))
      lengthTotalResult <- allTxQuery.length.result
      limitTotalResult <- allTxQuery.drop(offset).take(limit).result
    }
    yield (limitTotalResult, lengthTotalResult)
    Await.result(db.run(q), Duration.Inf)
  }

  /**
   * @param tokenId raffle token id
   * @return sum of refunded tickets belonging to the specified raffle
   */
  def refundedTickets(tokenId: String): Long ={
    val q = Txs.filter(tx => tx.tokenId === tokenId && tx.txType === refund.id).map(_.tokenCount).sum.result
    Await.result(db.run(q), Duration.Inf).getOrElse(0)
  }

  def byTxId(txId: String): TxCache = Await.result(db.run(Txs.filter(tx => tx.txId === txId).result.head), Duration.Inf)
  def refundByTxId(txId: String): TxCache = Await.result(db.run(Txs.filter(tx => tx.txId === txId && tx.txType === refund.id).result.head), Duration.Inf)

  /**
   * @param tokenId raffle token id
   * @return winner ticket of the specified raffle
   */
  def winnerByTokenId(tokenId: String): TxCache =
    Await.result(db.run(Txs.filter(tx => tx.tokenId === tokenId && tx.txType === winner.id).result.head), Duration.Inf)

  /**
   * @param walletAdd user wallet address
   * @return All donations done using the wallet address aggregated on each raffle
   */
  def getDonationsByWalletAddr(walletAdd: String, offset: Int, limit: Int): (Seq[(String, Option[Long])], Int) ={
    val query = for {
      walletTickets <- DBIO.successful(Txs.filter(tx => tx.walletAdd === walletAdd && tx.txType === ticket.id)
        .groupBy(_.tokenId).map{ case (tokenId, tx) => (tokenId, tx.map(_.tokenCount).sum)})
      walletTicketLength <- walletTickets.length.result
      walletTicketLimit <- walletTickets.drop(offset).take(limit).result
    } yield (walletTicketLimit, walletTicketLength)
    Await.result(db.run(query), Duration.Inf)
  }

  /**
   * @param walletAdd user wallet address
   * @return a seq of winner tickets belonging to this address
   */
  def winnerByWalletAddr(walletAdd: String, offset: Int, limit: Int): (Seq[TxCache], Int) = {
    val query = for {
      walletWins <- DBIO.successful(Txs.filter(tx => tx.walletAdd === walletAdd && tx.txType === winner.id))
      walletWinsLength <- walletWins.length.result
      walletWinsLimit <- walletWins.drop(offset).take(limit).result
    } yield (walletWinsLimit, walletWinsLength)
    Await.result(db.run(query), Duration.Inf)
  }
}




