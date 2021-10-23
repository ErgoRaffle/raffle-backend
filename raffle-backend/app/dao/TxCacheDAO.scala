package dao

import javax.inject.{Inject, Singleton}
import models.TxCache
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import slick.jdbc.JdbcProfile

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

trait TxCacheComponent {
  self: HasDatabaseConfigProvider[JdbcProfile] =>

  import profile.api._

  class TransactionsTable(tag: Tag) extends Table[TxCache](tag, "TRANSACTIONS") {
    def id = column[Long]("ID", O.PrimaryKey, O.AutoInc)
    def txId = column[String]("TX_ID")
    def tokenId = column[String]("TOKEN_ID")
    def tokenCount = column[Long]("TOKEN_COUNT")
    def txType = column[String]("TYPE")
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
  def insert(txId: String, tokenId: String, tokenCount: Long, txType: String,
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

  def byTokenId(tokenId: String, offset: Int, limit: Int): (Seq[TxCache], Seq[TxCache], Int, Int) = {
    val types = Seq("Winner", "Charity")
    val q = for {
      successTxsQ <- DBIO.successful(Txs.filter(tx => tx.tokenId === tokenId && (tx.txType inSet types)).sortBy(_.txType.asc))
      lengthTotalSuccessResult <- successTxsQ.length.result
      limitSuccessResult <- successTxsQ.drop(offset).take(limit).result
      otherQuery <- DBIO.successful(if (lengthTotalSuccessResult != 0) Txs.filter(tx => tx.tokenId === tokenId && !(tx.txType inSet types)) else Txs.filter(tx => tx.tokenId === tokenId && tx.txType === "Refund"))
      // In case of success raffle we have just 2 type Winner and Charity so for set offset in this case Math.max(0, offset - lengthTotalSuccessResult) used
      setLimitOffset <- if (lengthTotalSuccessResult != 0) otherQuery.drop(Math.max(0, offset - lengthTotalSuccessResult)).take(limit).result else otherQuery.drop(offset).take(limit).result
      total <- (otherQuery.length + lengthTotalSuccessResult).result
    }
    // TODO: After fixed fake charityTX should concat limitSuccessResult and setLimitOffset also remove lengthTotalSuccessResult
    yield (limitSuccessResult, setLimitOffset, lengthTotalSuccessResult, total)
    Await.result(db.run(q), Duration.Inf)
  }

  def byTxId(txId: String): TxCache = Await.result(db.run(Txs.filter(tx => tx.txId === txId).result.head), Duration.Inf)
  def refundByTxId(txId: String): TxCache = Await.result(db.run(Txs.filter(tx => tx.txId === txId && tx.txType === "Refund").result.head), Duration.Inf)

  def winnerByTokenId(tokenId: String): TxCache =
    Await.result(db.run(Txs.filter(tx => tx.tokenId === tokenId && tx.txType === "Winner").result.head), Duration.Inf)

  def byWalletAdd(walletAdd: String): Seq[TxCache] =
    Await.result(db.run(Txs.filter(tx => tx.walletAdd === walletAdd && tx.txType === "Ticket").result), Duration.Inf)
  def winnerByWalletAdd(walletAdd: String): Seq[TxCache] =
    Await.result(db.run(Txs.filter(tx => tx.walletAdd === walletAdd && tx.txType === "Winner").result), Duration.Inf)
}




