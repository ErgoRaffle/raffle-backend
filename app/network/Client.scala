package network


import helpers.{Configs, Utils, connectionException}
import org.ergoplatform.appkit.{Address, CoveringBoxes, ErgoClient, InputBox, RestApiErgoClient}
import play.api.Logger
import javax.inject.{Inject, Singleton}

import scala.collection.JavaConverters._

@Singleton
class Client @Inject()() {
  private val logger: Logger = Logger(this.getClass)
  private var client: ErgoClient = _

  /**
   * Sets client for the entire app when the app starts
   *
   * @return current height of blockchain
   */
  def setClient(): Long = {
    print("salam")
    try {
      client = RestApiErgoClient.create(Configs.nodeUrl, Configs.networkType, "", Configs.explorerUrl)
      client.execute(ctx => {
        ctx.getHeight
      })

    } catch {
      case e: Throwable =>
        logger.error(s"Could not set client! ${e.getMessage}.")
        0L
    }
  }

  def getClient: ErgoClient = {
    client
  }

  /**
   * @return current height of the blockchain
   */
  def getHeight: Long = {
    try {
      client.execute(ctx => ctx.getHeight)
    } catch {
      case e: Throwable =>
        logger.error(e.getMessage)
        throw connectionException()
    }
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes
   */
  def getUnspentBox(address: Address): List[InputBox] = {
    client.execute(ctx =>
      try {
        ctx.getUnspentBoxesFor(address, 0, 100).asScala.toList
      } catch {
        case e: Throwable =>
          logger.error(e.getMessage)
          throw connectionException()
      }
    )
  }

  def getAllUnspentBox(address: Address): List[InputBox] = {
    client.execute(ctx =>
      try {
        ctx.getCoveringBoxesFor(address, (1e9 * 1e8).toLong).getBoxes.asScala.toList
      } catch {
        case e: Throwable =>
          logger.error(e.getMessage)
          throw connectionException()
      }
    )
  }

  def getCoveringBoxesFor(address: Address, amount: Long): CoveringBoxes = {
    client.execute(ctx =>
      try {
        ctx.getCoveringBoxesFor(address, amount)
      } catch {
        case e: Throwable =>
          logger.error(e.getMessage)
          throw connectionException()
      }
    )
  }

}
