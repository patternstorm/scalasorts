package banking.accounts

import nats.{Naturals, NaturalsWithInts}
import universe.Universe._

trait AccountsImp {
  self: Accounts with Naturals with NaturalsWithInts =>

  case class AccountRep(balance: NatAsInt)

  implicit object AccountRep extends (account as AccountRep) {
    override def encode(acc: account.rep): AccountRep = acc match {
      case account.create.rep => AccountRep(NatAsInt(0))
      case account.deposit.rep(x, n) => {
        val m: NatAsInt = n
        AccountRep(NatAsInt(encode(x).balance.state + m.state))
      }
    }

    override def decode(acc: AccountRep): account.rep = if (acc.balance.state == 0) account.create.rep
    else account.deposit.rep(account.create.rep, acc.balance)
  }

  implicit object depositImp extends Implementation[account.deposit, account ->: nat ->: account, AccountRep => NatAsInt => AccountRep] {
    override def apply(): AccountRep => NatAsInt => AccountRep = acc => n => AccountRep(NatAsInt(acc.balance.state + n.state))
  }


}
