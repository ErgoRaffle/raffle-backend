package raffle

import javax.inject.Inject
import play.api.Logger

import java.security.SecureRandom
import scala.collection.JavaConverters._
import network.{Client, Explorer}
import sigmastate.interpreter.CryptoConstants.{dlogGroup, groupOrder}
import sigmastate.eval._
import special.sigma.GroupElement
import org.ergoplatform.appkit.{Address, ErgoToken, ErgoType, ErgoValue, InputBox, JavaHelpers, OutBox}
import helpers.{Configs, Utils}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import special.collection.Coll


class Adaptor @Inject()(client: Client, explorer: Explorer, utils: Utils, raffleContract: RaffleContract) {
  /*
  * adds a raffle
  * @param name
  * @param description
  * @param deadlineHeight
  * @param organizerAddr
  * @param charityAddr
  * @param minToRaise
  * @return tracnsactionId, in which new raffle added
  * */
  def addRaffle(name: String, description: String, deadlineHeight: Long, organizerAddr: String, charityAddr: String, minToRaise: Long): String = {
    client.getClient.execute(ctx => {

    }
  }


}