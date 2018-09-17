package banking.accounts

import nats.NaturalsWithInts
import universe.Universe._

trait AccountsImp {
  self: Accounts with NaturalsWithInts =>

  case class AccountRep(balance: NatAsInt)

  implicit object AccountRep extends (account as AccountRep) {
    override def encode(acc: account#rep): AccountRep = acc match {
      case account._create => AccountRep(NatAsInt(0))
      case account._deposit(x, n) => {
        val m: NatAsInt = n
        AccountRep(NatAsInt(encode(x).balance.state + m.state))
      }
    }

    override def decode(acc: AccountRep): account#rep = if (acc.balance.state == 0) account._create
    else account._deposit(account._create, acc.balance)
  }


}
