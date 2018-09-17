package banking.accounts


import nats.{Naturals, NaturalsWithInts}
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}


class AccountsSpec extends FunSpec with Matchers with GivenWhenThen
  with Accounts with Naturals with AccountsImp with NaturalsWithInts {

  describe("...") {
    it("...") {
      val m = account.deposit(account.create(), nat.succ(nat.zero()))
      val acc: AccountRep = m()
      acc.balance.state shouldEqual 1
    }
  }
}
