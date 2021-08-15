package models


case class CreateReq(id: Long, name: String, description: String, minToRaise: Long,
                     deadlineHeight: Long, winnerPercent: Int, charityAdd: String,
                     ticketPrice: Long, state: Int, paymentAddress: String,
                     proxyAddress: String, raffleAddress: String, raffleToken: String, newServiceBox: String,
                     signedProxyTxJson: Option[String],  signedCreateTxJson: Option[String],
                     chainedWith: Long, isChained: Boolean, timeOut: Long, ttl: Long)


case class DonateReq(id: Long, ticketCount: Long, ticketPrice: Long, state: Int, paymentAddress: String,
                     raffleAddress: String, raffleToken: String, signedDonateTxJson: Option[String],
                     participantAddress: String, ticketAddress: String, timeOut: Long, ttl: Long)


case class RefundReq(id: Long, ticketCount: Long, ticketPrice: Long, state: Int,
                     raffleAddress: String, raffleToken: String, signedRefundTxJson: Option[String],
                     ticketBoxId: String, timeOut: Long)

case class ActiveRaffle(id: Long, minToRaise: Long, deadlineHeight: Long, winnerPercent: Int,
                        charityAdd: String, ticketPrice: Long, state: Int, raffleAddress: String,
                        raffleToken: String, winnerBoxId: String, timeOut: Long, isUpdating: Boolean)