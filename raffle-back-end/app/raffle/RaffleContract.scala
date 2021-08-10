package raffle


import javax.inject.Inject

object RaffleContract {
  lazy val tokenIdService = "be5ecd5e083a82b11266e873cdac37c94b2c2cdeed3894ba9f9d16b8a8c879d8"
}

class RaffleContract @Inject()(/*client : Client*/){
//  val servicePubKey = "9faLY6U5W6RrCHdUJ4wSqctKYik9skpQDVDEETPqaX9uXWkjEva"
//  val serviceSecret = BigInt("51377199ca726394197759ebf50bf2d3dc73e464a422720921f734932164bf3e", 16)
//  val serviceAddress : Address = Address.create(servicePubKey)
//  val raffleProjectPubKey = "9guri1oTv5ZFH6uMgVgDp18jVVsJmTFoS9XvJyRm6n7vKSXouML"
//  val raffleProjectSecret = BigInt("4304f047d0e1c447001cb63e40ce59f8ccdd1dfabe925e39b54299a20c83240c", 16)
//  val raffleProjectAddress : Address = Address.create(raffleProjectPubKey)
//  val participantPubKey = "9ed9EeiuD8End4fHzTDSaGmqkRYWtS36UGqwNauxBGpmdAaUtg9"
//  val participantSecret = BigInt("ca7b4f0905c238cf752a22cbcb86ccf223c7fc2f0eb244641587b60f25c4a0ac", 16)
//  val participantAddress : Address = Address.create(participantPubKey)
//  val charityPubKey = "9hMYhAgWCyGmpKQLcLSzzXZ788SeQprntE5agz1T5XGKKrsxoGq"
//  val charitySecret = BigInt("370e394bbece18def71582b12283522aea3ca940af27c552d3508ef19c161f85", 16)
//  val charityAddress : Address = Address.create(charityPubKey)

//  val raffleTokenId: String = "298cbf467b7c5fd38fd3dd8cea35d6c3911f6960db6f6a66548f242a41742870"
//  val serviceTokenId: String = Configs.serviceTokenId
//  val oracleId = "011d3364de07e5a26f0c4eef0852cddb387039a921b7154ef3cab22c6eda887f"
//  val response: HttpResponse[String] = Http("https://api.ergoplatform.com/api/v1/boxes/unspent/byTokenId/").header("Accept", "application/json").asString



//  val oracleAddress =  "EfS5abyDe4vKFrJ48K5HnwTqa1ksn238bWFPe84bzVvCGvK1h2B7sgWLETtQuWwzVdBaoRZ1HcyzddrxLcsoM5YEy4UnqcLqMU1MDca1kLw9xbazAM6Awo9y6UVWTkQcS97mYkhkmx2Tewg3JntMgzfLWz5mACiEJEv7potayvk6awmLWS36sJMfXWgnEfNiqTyXNiPzt466cgot3GLcEsYXxKzLXyJ9EfvXpjzC2abTMzVSf1e17BHre4zZvDoAeTqr4igV3ubv2PtJjntvF2ibrDLmwwAyANEhw1yt8C8fCidkf3MAoPE6T53hX3Eb2mp3Xofmtrn4qVgmhNonnV8ekWZWvBTxYiNP8Vu5nc6RMDBv7P1c5rRc3tnDMRh2dUcDD7USyoB9YcvioMfAZGMNfLjWqgYu9Ygw2FokGBPThyWrKQ5nkLJvief1eQJg4wZXKdXWAR7VxwNftdZjPCHcmwn6ByRHZo9kb4Emv3rjfZE"


  lazy val ticketScript =
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

  lazy val winnerScript =
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

  lazy val tokenRepoScript =
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

  lazy val raffleServiceScript = // may not be used
    s"""{
       |  servicePubKey
       |}""".stripMargin

  lazy val donateScript =
    s"""{
       |  // proveDlog(decodePoint(pk)) ||)
       |  sigmaProp((OUTPUTS(1).R7[Coll[Byte]].get == pk) && (INPUTS(0).tokens(0)._1  == tokenId))
       |}""".stripMargin

}
