package raffle


import javax.inject.Inject

object RaffleContract {
  lazy val tokenIdService = "be5ecd5e083a82b11266e873cdac37c94b2c2cdeed3894ba9f9d16b8a8c879d8"
}

class RaffleContract @Inject()() {

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
       |  if (OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2) {
       |    ownerPK
       |  }else{
       |    if(OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2 + 1L){
       |      sigmaProp(
       |        allOf(
       |          Coll(
       |            OUTPUTS(0).R5[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |            OUTPUTS(0).R4[Long].get == SELF.R4[Long].get,
       |            OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |            OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |            OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |            OUTPUTS(0).value >= SELF.value,
       |          )
       |        )
       |      )
       |    }else{
       |      if(OUTPUTS.size == 5 && OUTPUTS(4).tokens.size > 0){
       |        // because output 4 can be available or not if this output exists and have tokens we can not continue and break down process
       |        sigmaProp(false)
       |      }else{
       |        // if we are here we have 4 outputs in transaction or 5th transaction have no tokens so
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              // at most we can have 5 output [service, raffle, token, fee, change]
       |              OUTPUTS.size <= 5,
       |              OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |              OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |              OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |              OUTPUTS(0).value >= SELF.value,
       |              OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2 - 1,
       |              OUTPUTS(0).R4[Long].get == SELF.R4[Long].get,
       |              OUTPUTS(0).R5[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |              blake2b256(OUTPUTS(1).propositionBytes) == raffleScriptHash,
       |              // [Charity Coef, service Fee, TicketPrice, Goal, Deadline, TotalSoldTicket]
       |              OUTPUTS(1).R4[Coll[Long]].get.size == 6,
       |              OUTPUTS(1).R4[Coll[Long]].get(1) == SELF.R4[Long].get,
       |              OUTPUTS(1).R4[Coll[Long]].get(0) > 0L,
       |              // Charity Coef + service fee < 100L (winner percent is required)
       |              OUTPUTS(1).R4[Coll[Long]].get(0) + OUTPUTS(1).R4[Coll[Long]].get(1) < 100L,
       |              // no sold ticket at begining
       |              OUTPUTS(1).R4[Coll[Long]].get(5) == 0,
       |              // Raffle charity address
       |              OUTPUTS(1).R5[Coll[Byte]].isDefined,
       |              // [Name, Description, pictures]
       |              OUTPUTS(1).R6[Coll[Coll[Byte]]].get.size >= 2,
       |              // service fee address stored in R7
       |              OUTPUTS(1).R7[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |              // must store ticket id to get all tokens from token repo box
       |              OUTPUTS(1).R8[Coll[Byte]].get == SELF.id,
       |              // Check Output token
       |              OUTPUTS(1).tokens(0)._1 == raffleServiceToken,
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

  lazy val ticketScript: String =
    s"""{
       |  val raffleSpend = {
       |    if (INPUTS.size == 3) {
       |      // winner reward. must pay back service token to servicebox we have 3 input boxes in this condition
       |      // Winner
       |      allOf(
       |        Coll(
       |          OUTPUTS(1).value == INPUTS(1).value,
       |          OUTPUTS(1).propositionBytes == SELF.R4[Coll[Byte]].get,
       |          INPUTS(1).tokens(0)._1 == raffleServiceToken,
       |          INPUTS(1).tokens(1)._1 == SELF.tokens(0)._1
       |        )
       |      )
       |    } else {
       |      if(INPUTS(0).tokens.size > 1) {
       |        // Refund
       |        allOf(
       |          Coll(
       |            INPUTS(0).tokens(1)._1 == SELF.tokens(0)._1,
       |            INPUTS(0).tokens(0)._1 == raffleServiceToken,
       |            INPUTS.size == 2,
       |            OUTPUTS(1).propositionBytes == SELF.R4[Coll[Byte]].get,
       |            OUTPUTS(1).value == SELF.R5[Coll[Long]].get(3) * SELF.tokens(0)._2
       |          )
       |        )
       |      } else {false}
       |    }
       |  }
       |  sigmaProp(raffleSpend) || (ownerPk && sigmaProp(HEIGHT > SELF.R5[Coll[Long]].get(2) + ExpireHeight))
       |}""".stripMargin

  lazy val RaffleScriptWaitingToken: String =
    s"""{
       |// inputs are [raffle_without_token, token_repo]
       |// outputs are [raffle_active]
       |sigmaProp(
       |  allOf(
       |    Coll(
       |      OUTPUTS(0).tokens(1)._1 == SELF.R8[Coll[Byte]].get,
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
       |    // user can donate
       |    val currentSoldTicket = OUTPUTS(1).tokens(0)._2
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          // validate app.raffle box
       |          OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(0).propositionBytes == SELF.propositionBytes,
       |          OUTPUTS(0).R5[Coll[Byte]].get == charityAddress,
       |          OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |          OUTPUTS(0).R7[Coll[Byte]].get == serviceAddress,
       |          outCharityCoef == charityCoef,
       |          outServiceFee == serviceFee,
       |          outTicketPrice == ticketPrice,
       |          outGoal == goal,
       |          outDeadlineHeight == deadlineHeight,
       |          outTotalSoldTicket == totalSoldTicket + currentSoldTicket,
       |          // check ticket script
       |          blake2b256(OUTPUTS(1).propositionBytes) == ticketScriptHash,
       |          OUTPUTS(1).tokens(0)._1 == SELF.tokens(1)._1,
       |          // protect token from burning
       |          SELF.tokens(1)._2 == OUTPUTS(0).tokens(1)._2 + currentSoldTicket,
       |          // check ergs
       |          OUTPUTS(1).value >= fee,
       |          OUTPUTS(0).value == SELF.value + (currentSoldTicket * ticketPrice),
       |          // Winner Address or redeem
       |          // TODO check R4 to be valid address
       |          OUTPUTS(1).R4[Coll[Byte]].isDefined,
       |          OUTPUTS(1).R6[Coll[Byte]].get == serviceAddress,
       |          // check ticket parameters [rangeStart, rangeEnd, deadlineHeight, ticketPrice]
       |          OUTPUTS(1).R5[Coll[Long]].get(0) == totalSoldTicket,
       |          OUTPUTS(1).R5[Coll[Long]].get(1) == outTotalSoldTicket,
       |          OUTPUTS(1).R5[Coll[Long]].get(2) == deadlineHeight,
       |          OUTPUTS(1).R5[Coll[Long]].get(3) == ticketPrice,
       |        )
       |      )
       |    )
       |  } else {
       |      if(totalRaised >= goal) {
       |        // charge charity address and service fee. then change status to completed
       |        val charityAmount = totalRaised * charityCoef / 100L
       |        val serviceFeeAmount = totalRaised * serviceFee / 100L
       |        val winnerAmount = totalRaised - charityAmount - serviceFeeAmount
       |        val winNumber = (((byteArrayToBigInt(CONTEXT.dataInputs(0).id.slice(0, 15)).toBigInt % totalSoldTicketBI) + totalSoldTicketBI) % totalSoldTicketBI).toBigInt
       |        sigmaProp(
       |          allOf(
       |            Coll(
       |              // check winner box remain on output box
       |              blake2b256(OUTPUTS(0).propositionBytes) == winnerScriptHash,
       |              OUTPUTS(0).R4[Coll[Long]].get == SELF.R4[Coll[Long]].get,
       |              OUTPUTS(0).R5[Coll[Byte]].get == charityAddress,
       |              OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |              OUTPUTS(0).R7[Coll[Byte]].get == serviceAddress,
       |
       |              OUTPUTS(0).R8[Long].get == winNumber,
       |              OUTPUTS(0).tokens(0)._1 == SELF.tokens(0)._1,
       |              OUTPUTS(0).tokens(1)._1 == SELF.tokens(1)._1,
       |              OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2,
       |              OUTPUTS(0).value >= winnerAmount,
       |              // check charity to passed to charity address
       |              OUTPUTS(1).propositionBytes == charityAddress,
       |              OUTPUTS(1).value >= charityAmount,
       |              // check service fee
       |              OUTPUTS(2).propositionBytes == serviceAddress,
       |              OUTPUTS(2).value >= serviceFeeAmount,
       |              // check datainput to be oracle box
       |              CONTEXT.dataInputs(0).tokens(0)._1 == randomBoxToken,
       |              // and datainput must created after deadline
       |              CONTEXT.dataInputs(0).creationInfo._1 > deadlineHeight,
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
       |              OUTPUTS(0).R4[Coll[Long]].get == SELF.R4[Coll[Long]].get,
       |              // box must move to refund state
       |              OUTPUTS(0).R5[Coll[Byte]].get == charityAddress,
       |              OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
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
       |  // Input boxes are [ Service, Winner, Ticket]
       |  // Output boxes are [ Service, winner prize]
       |  val winNumber = SELF.R8[Long].get
       |  sigmaProp(
       |    allOf(
       |      Coll(
       |        OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |        OUTPUTS(0).tokens(1)._1 == raffleServiceToken,
       |        OUTPUTS(0).tokens(1)._2 == SELF.tokens(0)._2 + INPUTS(0).tokens(1)._2,
       |        INPUTS(2).tokens(0)._1 == SELF.tokens(1)._1,
       |        INPUTS(2).R5[Coll[Long]].get(0) <= winNumber,
       |        INPUTS(2).R5[Coll[Long]].get(1) > winNumber,
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
       |    // inputs are [Service, raffle]
       |    // outputs are [Service]
       |    sigmaProp(
       |      allOf(
       |        Coll(
       |          OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |          OUTPUTS(0).tokens(1)._1 == SELF.tokens(0)._1,
       |          OUTPUTS(0).tokens(1)._2 == SELF.tokens(0)._2 + INPUTS(0).tokens(1)._2
       |        )
       |      )
       |    )
       |  } else {
       |    // inputs [ Raffle, Ticket]
       |    // Outputs[ Raffle, Refund]
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
       |          OUTPUTS(0).R5[Coll[Byte]].get == SELF.R5[Coll[Byte]].get,
       |          OUTPUTS(0).R6[Coll[Coll[Byte]]].get == SELF.R6[Coll[Coll[Byte]]].get,
       |          OUTPUTS(0).tokens(0)._1 == raffleServiceToken,
       |          OUTPUTS(0).tokens(1)._1 == SELF.tokens(1)._1,
       |          // validate Token & ERG
       |          OUTPUTS(0).value == SELF.value - (INPUTS(1).tokens(0)._2 * ticketPrice),
       |          OUTPUTS(0).tokens(1)._2 == SELF.tokens(1)._2 + INPUTS(1).tokens(0)._2,
       |        )
       |      )
       |    )
       |  }
       |}""".stripMargin

  lazy val createRaffleProxyScript: String =
    s"""{
       |  if(OUTPUTS.size > 2) {
       |    val createRaffleConditions = {
       |      allOf(Coll(
       |        OUTPUTS(0).tokens(0)._1 == raffleServiceNFT,
       |        OUTPUTS(1).tokens(0)._1 == raffleServiceToken,
       |        OUTPUTS(1).R4[Coll[Long]].get(0) == charityCoef,
       |        OUTPUTS(1).R4[Coll[Long]].get(2) == ticketPrice,
       |        OUTPUTS(1).R4[Coll[Long]].get(3) == goal,
       |        OUTPUTS(1).R4[Coll[Long]].get(4) == deadlineHeight,
       |        OUTPUTS(1).R5[Coll[Byte]].get == charityAddress,
       |        OUTPUTS(1).R6[Coll[Coll[Byte]]].get(0) == name,
       |        OUTPUTS(1).R6[Coll[Coll[Byte]]].get(1) == description,
       |        %s
       |      ))
       |    }
       |    sigmaProp(createRaffleConditions)
       |  }
       |  else {
       |    val returnCreateRaffleFee = {
       |      allOf(Coll(
       |        OUTPUTS(0).propositionBytes == userAddress, // user must receive the appropriate amount, only refund transactions's fee must be deducted from user's funds
       |        HEIGHT > refundHeightThreshold, // The create raffle confirmation block has passed the refundHeightThreshold
       |        OUTPUTS.size == 2, // only refund box and transaction fee box is needed
       |        OUTPUTS(1).value <= maxFee
       |      ))
       |    }
       |    sigmaProp(returnCreateRaffleFee)
       |  }
       |}""".stripMargin

  lazy val donateScript: String =
    s"""{
       |  if(OUTPUTS.size > 2) {
       |    val donateConditions = (OUTPUTS(1).R4[Coll[Byte]].get == userAddress) && (INPUTS(0).tokens(1)._1  == tokenId) && (OUTPUTS(1).tokens(0)._2 == ticketCount)
       |    sigmaProp(donateConditions)
       |  }
       |  else {
       |    val returnDonates = {
       |      allOf(Coll(
       |        OUTPUTS(0).propositionBytes == userAddress, // user must receive the appropriate amount, only refund transactions's fee must be deducted from user's funds
       |        (HEIGHT > refundHeightThreshold || HEIGHT >= raffleDeadline), // The donate request confirmation block has passed the refundHeightThreshold or raffleDeadline passed
       |        OUTPUTS.size == 2, // only refund box and transaction fee box is needed
       |        OUTPUTS(1).value <= maxFee,
       |      ))
       |    }
       |    sigmaProp(returnDonates)
       |  }
       |}""".stripMargin

}
