package raffle


import javax.inject.Inject

object RaffleContract {
  lazy val tokenIdService = "be5ecd5e083a82b11266e873cdac37c94b2c2cdeed3894ba9f9d16b8a8c879d8"

}

class RaffleContract @Inject()(){

  lazy val raffleTokenIssueRepo: String =
    s"""{
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        OUTPUTS(0).tokens(0)._1 == raffleServiceToken,
       |        OUTPUTS(0).tokens(1)._1 == SELF.tokens(0)._1,
       |        OUTPUTS(0).tokens(1)._2 == SELF.tokens(0)._2,
       |        SELF.tokens(0)._1 == INPUTS(0).R8[Coll[Byte]].get
       |      )
       |    )
       |  )
       |}""".stripMargin

  lazy val RaffleServiceScript: String =
    s"""{
       |  if (OUTPUTS(0).propositionBytes == SELF.R5[Coll[Byte]].get) {
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OUTPUTS(0).propositionBytes == SELF.R5[Coll[Byte]].get,
       |          OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |          OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |          OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2,
       |          OUTPUTS(0).value >= SELF.value - minFee
       |        )
       |      )
       |    )
       |  }else{
       |    if(OUTPUTS(0).tokens(1)._2 > SELF.tokens(1)._2){
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            true,
       |            OUTPUTS(0).R5[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |            OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |            OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |            OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |            OUTPUTS(0).value >= SELF.value,
//       |            INPUTS.size == 2,
       |            OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2 + INPUTS(1).tokens(0)._2,
       |          )
       |        )
       |      )
       |    }else{
       |      if(OUTPUTS.size == 5 && OUTPUTS(4).tokens.size > 0){
       |        // because output 4 can be available or not if this output exists and have tokens we can not continue and break down process
       |        sigmaProp(true)
       |      }else{
       |        // if we are here we have 4 outputs in transaction or 5th transaction have no tokens so
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              // atmost we can have 5 output [service, raffle, token, fee, change]
       |              OUTPUTS.size <= 5,
       |              OUTPUTS.size >= 4,
       |              OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |              OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |              OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |              OUTPUTS(0).value >= SELF.value,
       |              OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2 - 1,
       |              OUTPUTS(0).R4[Long] == SELF.R4[Long],
       |              OUTPUTS(0).R5[Coll[Byte]] == SELF.R5[Coll[Byte]],
       |              blake2b256(OUTPUTS(1).propositionBytes) == raffleScriptHash,
       |              OUTPUTS(1).R7[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |              OUTPUTS(1).R4[Coll[Long]].get.size == 6,
       |              OUTPUTS(1).R4[Coll[Long]].get(1) == SELF.R4[Long].get,
       |              // Charity Coef + service fee < 100L (winner percent is required)
       |              OUTPUTS(1).R4[Coll[Long]].get(0) + OUTPUTS(1).R4[Coll[Long]].get(1) < 100L,
       |              // no sold ticket at begining
       |              OUTPUTS(1).R4[Coll[Long]].get(5) == 0,
       |              // status set to zero (create token)
       |              OUTPUTS(1).R5[Coll[Byte]].isDefined,
       |              OUTPUTS(1).R6[Coll[Coll[Byte]]].get.size == 3,
       |              // service fee address stored in R7
       |              OUTPUTS(1).R7[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |              // must store ticket id to get all tokens from token repo box
       |              OUTPUTS(1).R8[Coll[Byte]].get == SELF.id,
       |              // third output must be token repo
       |              blake2b256(OUTPUTS(2).propositionBytes) == raffleTokenIssueHash,
       |              OUTPUTS(2).tokens(0)._1 == SELF.id,
       |              OUTPUTS(3).tokens.size == 0
       |            )
       |          )
       |        )
       |      }
       |    }
       |  }
       |}""".stripMargin

  lazy val ticketScript : String =
    s"""{
       |  //winner reward. must pay back service token to servicebox we have 3 input boxes in this condition
       |  if (HEIGHT < SELF.R5[Coll[Long]].get(2)){
       |    // raffle does not completed
       |    sigmaProp(false)
       |  }else if (INPUTS.size == 3) {
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OUTPUTS(1).value == INPUTS(1).value,
       |          OUTPUTS(1).propositionBytes == SELF.R4[Coll[Byte]].get,
       |          INPUTS(2).id == SELF.id,
       |          INPUTS(1).tokens(0)._1 == raffleServiceToken,
       |          INPUTS(1).tokens(1)._1 == SELF.tokens(0)._1
       |        )
       |      )
       |    )
       |  } else {
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          true,
       |          INPUTS(0).tokens(1)._1 == SELF.tokens(0)._1,
       |          INPUTS(0).tokens(0)._1 == raffleServiceToken,
       |          INPUTS(1).id == SELF.id,
       |          INPUTS.size == 2,
       |          OUTPUTS(1).propositionBytes == SELF.R4[Coll[Byte]].get,
       |          OUTPUTS(1).value == SELF.R5[Coll[Long]].get(3) * SELF.tokens(0)._2 - fee
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val RaffleScriptWaitingToken: String =
    s"""{
       |// inputs are [raffle_without_token, token_repo]
       |// outputs are [raffle_active]
       |sigmaProp(
       |  allOf(
       |    Coll(
       |      true,
       |      SELF.id == INPUTS(0).id,
       |      OUTPUTS(0).value == SELF.value,
       |      blake2b256(OUTPUTS(0).propositionBytes) == raffleActiveHash,
       |      OUTPUTS(0).R4[Coll[Long]].get == SELF.R4[Coll[Long]].get,
       |      OUTPUTS(0).R5[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |      OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |      OUTPUTS(0).R7[Coll[Byte]].get == SELF.R7[Coll[Byte]].get,
       |    )
       |  )
       |)
       |}""".stripMargin

  lazy val raffleActiveScript: String =
    s"""{
       |  val charityCoef = SELF.R4[Coll[Long]].get(0)
       |  val serviceFee = SELF.R4[Coll[Long]].get(1)
       |  val ticketPrice = SELF.R4[Coll[Long]].get(2)
       |  val goal = SELF.R4[Coll[Long]].get(3)
       |  val deadlineHeight = SELF.R4[Coll[Long]].get(4)
       |  val totalSoldTicket = SELF.R4[Coll[Long]].get(5)
       |  val totalSoldTicketBI: BigInt = totalSoldTicket.toBigInt
       |  val winnerCoef = 100L - charityCoef - serviceFee
       |  val charityAddress = SELF.R5[Coll[Byte]].get
       |  val serviceAddress = SELF.R7[Coll[Byte]].get
       |  val totalRaised = totalSoldTicket * ticketPrice
       |  val outCharityCoef = OUTPUTS(0).R4[Coll[Long]].get(0)
       |  val outServiceFee = OUTPUTS(0).R4[Coll[Long]].get(1)
       |  val outTicketPrice = OUTPUTS(0).R4[Coll[Long]].get(2)
       |  val outGoal = OUTPUTS(0).R4[Coll[Long]].get(3)
       |  val outDeadlineHeight = OUTPUTS(0).R4[Coll[Long]].get(4)
       |  val outTotalSoldTicket = OUTPUTS(0).R4[Coll[Long]].get(5)
       |  if (HEIGHT < deadlineHeight) {
       |    val currentSoldTicket = OUTPUTS(1).tokens(0)._2
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          true,
       |          // validate app.raffle box
       |          OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |          OUTPUTS(0).R5[Coll[Byte]] == SELF.R5[Coll[Byte]],
       |          OUTPUTS(0).R6[Coll[Coll[Byte]]] == SELF.R6[Coll[Coll[Byte]]],
       |          OUTPUTS(0).R7[Coll[Byte]] == SELF.R7[Coll[Byte]],
       |          outCharityCoef == charityCoef,
       |          outServiceFee == serviceFee,
       |          outTicketPrice == ticketPrice,
       |          outGoal == goal,
       |          outDeadlineHeight == deadlineHeight,
       |          outTotalSoldTicket == totalSoldTicket + currentSoldTicket,
       |          // check ticket script
       |          blake2b256(OUTPUTS(1).propositionBytes) == ticketScriptHash,
       |          OUTPUTS(1).tokens.size == 1,
       |          OUTPUTS(1).tokens(0)._1 == SELF.tokens(1)._1,
       |          // protect token from burning
       |          SELF.tokens(1)._2 == OUTPUTS(0).tokens(1)._2 + OUTPUTS(1).tokens(0)._2,
       |          // check ergs
       |          OUTPUTS(0).value > SELF.value,
       |          OUTPUTS(1).value >= fee,
       |          OUTPUTS(0).value == SELF.value + currentSoldTicket * ticketPrice,
       |          // check ticket parameters
       |          OUTPUTS(1).R5[Coll[Long]].get(0) == totalSoldTicket,
       |          OUTPUTS(1).R5[Coll[Long]].get(1) == outTotalSoldTicket,
       |          OUTPUTS(1).R5[Coll[Long]].get(2) == deadlineHeight,
       |          OUTPUTS(1).R5[Coll[Long]].get(3) == ticketPrice,
       |        )
       |      )
       |    )
       |  } else {
       |      if(SELF.value >= goal) {
       |        // charge charity address and service fee. then change status to completed
       |        val charityAmount = totalRaised * charityCoef / 100L
       |        val serviceFeeAmount = totalRaised * serviceFee / 100L
       |        val winnerAmount = totalRaised - charityAmount - serviceFeeAmount + fee
       |        val winNumber = ((byteArrayToBigInt(CONTEXT.dataInputs(0).id.slice(0, 15)).toBigInt + totalSoldTicketBI) % totalSoldTicketBI).toBigInt
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |            true,
       |              // TODO must check data input to be oracle box
       |              // check winner box remain on output box
       |              blake2b256(OUTPUTS(0).propositionBytes) == winnerScriptHash,
       |              OUTPUTS(0).R4[Coll[Long]].get(0) == charityCoef,
       |              OUTPUTS(0).R4[Coll[Long]].get(1) == serviceFee,
       |              OUTPUTS(0).R4[Coll[Long]].get(2) == ticketPrice,
       |              OUTPUTS(0).R4[Coll[Long]].get(3) == goal,
       |              OUTPUTS(0).R4[Coll[Long]].get(4) == deadlineHeight,
       |              OUTPUTS(0).R4[Coll[Long]].get(5) == totalSoldTicket,
       |              OUTPUTS(0).R5[Coll[Byte]] == SELF.R5[Coll[Byte]],
       |              OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |              OUTPUTS(0).R7[Coll[Byte]].get == SELF.R7[Coll[Byte]].get,

       |              OUTPUTS(0).R8[Long].get == winNumber,
       |              OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |              OUTPUTS(0).tokens(1)._1 == SELF.tokens(1)._1,
       |              OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2,
       |              OUTPUTS(0).value == winnerAmount,
       |              // check charity to passed to charity address
       |              OUTPUTS(1).propositionBytes == charityAddress,
       |              OUTPUTS(1).value == charityAmount,
       |              // check service fee
       |              OUTPUTS(2).propositionBytes == serviceAddress,
       |              OUTPUTS(2).value == serviceFeeAmount
       |            )
       |          )
       |        )
       |      } else {
       |      // begin refund
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              // check winner box remain on output box
       |              blake2b256(OUTPUTS(0).propositionBytes) == redeemScriptHash,
       |              OUTPUTS(0).R4[Coll[Long]].get(0) == charityCoef,
       |              OUTPUTS(0).R4[Coll[Long]].get(1) == serviceFee,
       |              OUTPUTS(0).R4[Coll[Long]].get(2) == ticketPrice,
       |              OUTPUTS(0).R4[Coll[Long]].get(3) == goal,
       |              OUTPUTS(0).R4[Coll[Long]].get(4) == deadlineHeight,
       |              OUTPUTS(0).R4[Coll[Long]].get(5) == totalSoldTicket,
       |              // box must move to refund state
       |              OUTPUTS(0).R5[Coll[Byte]] == SELF.R5[Coll[Byte]],
       |              OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |              OUTPUTS(0).R7[Coll[Byte]] == SELF.R7[Coll[Byte]],
       |              OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |              OUTPUTS(0).tokens(1)._1 == SELF.tokens(1)._1,
       |              OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2,
       |              OUTPUTS(0).value == SELF.value - fee,
       |            )
       |          )
       |        )
       |      }
       |    }
       |}""".stripMargin

  lazy val raffleWinnerScript: String =
    s"""{
       |  val winNumber = SELF.R8[Long].get
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        true,
       |        OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |        OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |        OUTPUTS(0).tokens(1)._2 == SELF.tokens(0)._2 + INPUTS(0).tokens(1)._2,
       |        INPUTS(1).id == SELF.id,
       |        INPUTS(2).tokens(0)._1 == SELF.tokens(1)._1,
       |        INPUTS(2).R5[Coll[Long]].get(0) < winNumber,
       |        INPUTS(2).R5[Coll[Long]].get(0) + INPUTS(2).R5[Coll[Long]].get(1) >= winNumber,
       |      )
       |    )
       |  )
       |}""".stripMargin

  lazy val raffleRedeemScript: String =
    s"""{
       |  val ticketPrice = SELF.R4[Coll[Long]].get(2)
       |  val totalSoldTicket = SELF.R4[Coll[Long]].get(5)
       |  // refunding process
       |  if(totalSoldTicket == 0){
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          true,
       |          OUTPUTS(0).tokens(0)._1 ==raffleServiceNFT,
       |          OUTPUTS(0).tokens(1)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(0).tokens(1)._2 == SELF.tokens(0)._2 + INPUTS(0).tokens(1)._2
       |        )
       |      )
       |    )
       |  } else {
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |          OUTPUTS(0).R4[Coll[Long]].get(0) == SELF.R4[Coll[Long]].get(0),
       |          OUTPUTS(0).R4[Coll[Long]].get(1) == SELF.R4[Coll[Long]].get(1),
       |          OUTPUTS(0).R4[Coll[Long]].get(2) == ticketPrice,
       |          OUTPUTS(0).R4[Coll[Long]].get(3) == SELF.R4[Coll[Long]].get(3),
       |          OUTPUTS(0).R4[Coll[Long]].get(4) == SELF.R4[Coll[Long]].get(4),
       |          OUTPUTS(0).R4[Coll[Long]].get(5) == totalSoldTicket - INPUTS(1).tokens(0)._2,
       |          OUTPUTS(0).R5[Coll[Byte]] == SELF.R5[Coll[Byte]],
       |          OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |          OUTPUTS(0).R7[Coll[Byte]].get == SELF.R7[Coll[Byte]].get,
       |          OUTPUTS(0).tokens(0)._1 == raffleServiceToken,
       |          OUTPUTS(0).tokens(1)._1 == SELF.tokens(1)._1,
       |          // validate Token & ERG
       |          OUTPUTS(0).value == SELF.value - INPUTS(1).tokens(0)._2 * ticketPrice,
       |          OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2 + INPUTS(1).tokens(0)._2,
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val createRaffleProxyScript: String =
    s"""{
       |  PK ||
       |  sigmaProp(
       |    allOf(
       |      Coll(
//       |        true,
       |        OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |        OUTPUTS(1).tokens(0)._1 == raffleServiceToken,
       |        OUTPUTS(1).R4[Coll[Long]].get(0) == charityCoef,
       |        OUTPUTS(1).R4[Coll[Long]].get(2) == ticketPrice,
       |        OUTPUTS(1).R4[Coll[Long]].get(3) == goal,
       |        OUTPUTS(1).R4[Coll[Long]].get(4) == deadlineHeight,
//       |        OUTPUTS(1).R5[Coll[Byte]].get == charityAddress,
       |      )
       |    )
       |  )
       |}""".stripMargin


  // OLD CONTRACT
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
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        OUTPUTS(1).tokens(1)._1 == serviceToken,
       |        blake2b256(OUTPUTS(1).propositionBytes) == raffleHash,
       |        OUTPUTS(1).tokens(0)._1 == SELF.tokens(0)._1,
       |        OUTPUTS(1).tokens(0)._2 == SELF.tokens(0)._2
       |      )
       |    )
       |  )
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
