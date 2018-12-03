package banking.accounts


import nats.NATasLong
import universe.Universe._
import ACCOUNT._
import nats.NAT._


//@representation[account]
//case class InvestmentAccount(balance: NATasLong) {
//  def create(): InvestmentAccount = InvestmentAccount(NATasLong(0))
//  def deposit(account: InvestmentAccount, amount: NATasLong): InvestmentAccount = InvestmentAccount(NATasLong(account.balance.value + amount.value))
//  (x: InvestmentAccount) => account = if (x.balance.value == 0) account.create() else account.deposit(account.create(), x.balance) //TODO implicit conversion here
//}

//TODO DSL statements to define implementations
object InvestmentAccount extends account.impl {

  case class rep(balance: NATasLong.rep) {
    override def toString: String = symbol + "(balance=" + balance + ")"
  }

  override val symbol: String = "InvestmentAccount"
  implicit val me: InvestmentAccount.type = this

  implicit def unlift(a: Rep[InvestmentAccount.type]): rep = a.value

  override protected def _decode(x: rep): account.type#rep =
    if (x.balance.value == 0) account.create.rep else account.deposit.rep(account.create.rep, NATasLong._decode(x.balance))

  override protected def _encode(x: account.create.rep): rep = InvestmentAccountImp.create()

  override protected def _encode(x: account.deposit.rep): rep = InvestmentAccountImp.deposit(x._1, x._2)

  def apply(value: NATasLong.rep): rep = rep(value)
}

object InvestmentAccountImp extends
  account.create.impl[InvestmentAccount.type] with
  account.deposit.impl[InvestmentAccount.type, NATasLong.type, InvestmentAccount.type] with
  account.balance.impl[InvestmentAccount.type, NATasLong.type] {

  override def create(): InvestmentAccount.rep = InvestmentAccount(NATasLong(0))

  override def deposit(account: InvestmentAccount.rep, amount: NATasLong.rep): InvestmentAccount.rep = InvestmentAccount(NATasLong(account.balance.value + amount.value))

  override def balance(account: InvestmentAccount.rep): NATasLong.rep = account.balance

}
