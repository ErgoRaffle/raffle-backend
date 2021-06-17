package network


import helpers.{Configs, Utils}
import javax.inject.{Inject, Singleton}
import org.ergoplatform.appkit.{Address, ErgoClient, InputBox, JavaHelpers, NetworkType, RestApiErgoClient}

import scala.collection.JavaConverters._
import play.api.Logger

import scala.concurrent.BlockContext

@Singleton
class Client @Inject()(utils: Utils) {
  private val logger: Logger = Logger(this.getClass)
  private val defaultHeader: Seq[(String, String)] = Seq[(String, String)](("Content-Type", "application/json"), ("api_key", Configs.nodeApiKey))
  private var client: ErgoClient = _

  /**
   * Sets client for the entire app when the app starts
   *
   * @return current height of blockchain
   */
  def setClient(): Long = {
    print("salam")
    try {
      client = RestApiErgoClient.create(Configs.nodeUrl, Configs.networkType, Configs.nodeApiKey)
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
    return client
  }

  /**
   * @return current height of the blockchain
   */
  def getHeight: Long = {
    client.execute(ctx => ctx.getHeight)
  }

  /**
   * @param address :Address get a valid address
   * @return List of input address boxes
   */
  def getUnspentBox(address: Address): List[InputBox] = {
    client.execute(ctx =>
      ctx.getUnspentBoxesFor(address).asScala.toList
    )
  }

}