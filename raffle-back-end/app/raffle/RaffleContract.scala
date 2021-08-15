package raffle


import javax.inject.Inject

object RaffleContract {
  lazy val tokenIdService = "be5ecd5e083a82b11266e873cdac37c94b2c2cdeed3894ba9f9d16b8a8c879d8"
}

class RaffleContract @Inject()(){

  lazy val ticketScript : String =
    s"""{
       |  val refundPhaseSpend = HEIGHT > deadlineHeight &&
       |                         blake2b256(INPUTS(0).propositionBytes) == SELF.R6[Coll[Byte]].get &&
       |                         INPUTS(0).tokens(0)._1 == SELF.tokens(0)._1
       |
       |  val winnerPhaseSpend = HEIGHT > deadlineHeight &&
       |                         blake2b256(INPUTS(0).propositionBytes) == winnerScriptHash &&
       |                         INPUTS(0).tokens(0)._1 == SELF.tokens(0)._1
       |
       |  val receiverCheck = OUTPUTS(1).propositionBytes  == SELF.R7[Coll[Byte]].get &&
       |                      OUTPUTS(1).value == SELF.tokens(0)._2 * ticketPrice &&
       |                      INPUTS.size == 2
       |
       |  val receiverCheckWinner = OUTPUTS(0).propositionBytes == SELF.R7[Coll[Byte]].get &&
       |                            OUTPUTS(0).value == INPUTS(0).value
       |
       |  sigmaProp((receiverCheck && refundPhaseSpend) || (receiverCheckWinner && winnerPhaseSpend))
       |}""".stripMargin

  lazy val winnerScript : String =
    s"""{
       |  sigmaProp(
       |    allOf(Coll(
       |          // Valid Ticket
       |          INPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |          INPUTS(1).R4[Long].get <= SELF.R4[Long].get,
       |          INPUTS(1).R4[Long].get + INPUTS(1).R5[Long].get > SELF.R4[Long].get
       |    ))
       |  )
       |}""".stripMargin

  lazy val tokenRepoScript : String =
    s"""{
       |  val totalSoldTicket = SELF.R4[Long].get
       |  val totalSoldTicketBI: BigInt = totalSoldTicket.toBigInt
       |  val totalRaised = totalSoldTicket * ticketPrice
       |  val charityCoef = SELF.R5[Long].get
       |  val projectCoef = SELF.R6[Long].get
       |  val winnerCoef = 100L - charityCoef - projectCoef
       |  sigmaProp(
       |    if (HEIGHT < deadlineHeight) {
       |      allOf(Coll(
       |            // validate Script
       |            OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |            blake2b256(OUTPUTS(1).propositionBytes) == ticketScriptHash,
       |            OUTPUTS(1).R6[Coll[Byte]].get == blake2b256(SELF.propositionBytes),
       |            // minERG
       |            INPUTS(1).value >= ticketPrice + 2 * minFee,
       |            // validate Register
       |            OUTPUTS(0).R4[Long].get == totalSoldTicket + (INPUTS(1).value - 2 * minFee) / ticketPrice,
       |            OUTPUTS(1).R4[Long].get == totalSoldTicket,
       |            OUTPUTS(1).R5[Long].get == (INPUTS(1).value - 2 * minFee) / ticketPrice,
       |            // validate Token
       |            OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |            OUTPUTS(0).tokens(0)._2 == SELF.tokens(0)._2 - (INPUTS(1).value - 2 * minFee) / ticketPrice,
       |            OUTPUTS(0).tokens(1)._1 == SELF.tokens(1)._1, // Raffle Service Token
       |            OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |            OUTPUTS(1).tokens(0)._2 == (INPUTS(1).value - 2 * minFee) / ticketPrice,
       |            // ERG Protect
       |            OUTPUTS(0).value == SELF.value + INPUTS(1).value - 2 * minFee,
       |            OUTPUTS(1).value == minFee,
       |            // same Coef
       |            OUTPUTS(0).R5[Long].get == charityCoef,
       |            OUTPUTS(0).R6[Long].get == projectCoef,
       |            true
       |            ))
       |    }
       |    else {
       |      if (totalRaised >= minToRaise) {
       |        allOf(Coll(
       |              // Validate Size
       |              INPUTS.size == 1 && OUTPUTS.size == 5,
       |              // Pay Back Raffle Service Token
       |              OUTPUTS(0).tokens(0)._1 == SELF.tokens(1)._1,
       |              OUTPUTS(0).tokens(0)._2 == 1,
       |              OUTPUTS(0).propositionBytes == servicePubKey.propBytes,
       |              // Charity Box
       |              OUTPUTS(1).value >= totalRaised * charityCoef / 100,
       |              OUTPUTS(1).propositionBytes == charityPubKey.propBytes,
       |              // TODO: Change here
       |              // Project Box
       |              OUTPUTS(2).value >= totalRaised * projectCoef / 100,
       |              OUTPUTS(2).propositionBytes == servicePubKey.propBytes,
       |              // Validate Seed
       |              CONTEXT.dataInputs(0).tokens(0)._1 == oracleNebulaNFT,
       |              // Winner Box
       |              OUTPUTS(3).value >= totalRaised * winnerCoef / 100,
       |              blake2b256(OUTPUTS(3).propositionBytes) == winnerScriptHash,
       |              OUTPUTS(3).R4[Long].get == ((byteArrayToBigInt(CONTEXT.dataInputs(0).id.slice(0, 15)).toBigInt + totalSoldTicketBI) % totalSoldTicketBI).toBigInt,
       |              OUTPUTS(3).tokens(0)._1 == SELF.tokens(0)._1,
       |              OUTPUTS(3).tokens(0)._2 == SELF.tokens(0)._2
       |         ))
       |      }
       |      else {
       |      if (totalRaised < minToRaise) {
       |        if(totalSoldTicket > 0){
       |          allOf(Coll(
       |                // validate Script
       |                OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |                // validate Token & ERG
       |                OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |                OUTPUTS(0).value >= SELF.value - (OUTPUTS(0).tokens(0)._2 - SELF.tokens(0)._2) * ticketPrice,
       |                OUTPUTS(0).tokens(0)._2 > SELF.tokens(0)._2,
       |                OUTPUTS(0).R4[Long].get == SELF.R4[Long].get - (OUTPUTS(0).tokens(0)._2 - SELF.tokens(0)._2)
       |          ))
       |        }
       |        else
       |        {
       |          allOf(Coll(
       |                // Pay Back Raffle Service Token
       |                OUTPUTS(0).tokens(0)._1 == SELF.tokens(1)._1,
       |                OUTPUTS(0).tokens(0)._2 == 1,
       |                OUTPUTS(0).propositionBytes == servicePubKey.propBytes
       |          ))
       |        }
       |      }
       |      else {
       |        false
       |      }
       |    }
       |  })
       |}""".stripMargin

  lazy val raffleServiceScript : String = // may not be used
    s"""{
       |  servicePubKey
       |}""".stripMargin

  lazy val donateScript : String =
    s"""{
       |  proveDlog(decodePoint(pk)) ||
       |  sigmaProp(//(OUTPUTS(1).R7[Coll[Byte]].get == pk) &&
       |            (INPUTS(0).tokens(0)._1  == tokenId) &&
       |            (OUTPUTS(1).tokens(0)._2 == ticketCount))
       |}""".stripMargin

  lazy val newRaffleProxyScript : String =
    s"""{
       |  sigmaProp(allOf(Coll(OUTPUTS(1).tokens(1)._1 == serviceToken,
       |                       blake2b256(OUTPUTS(1).propositionBytes) == raffleHash,
       |                       OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |                       OUTPUTS(1).tokens(0)._2 == SELF.tokens(0)._2
       |            )))
       |}""".stripMargin

  lazy val rafflePaymentScript : String =
  s"""
     |  pk ||
     |  sigmaProp(allOf(Coll(blake2b256(OUTPUTS(0).propositionBytes) == raffleProxyHash,
     |                       OUTPUTS(0).tokens(0)._1 == SELF.id,
     |                       OUTPUTS(0).tokens(0)._2 == 1000000000000000000L
     |            )))
     |""".stripMargin
}
