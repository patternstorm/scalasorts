package banking.accounts


import org.scalatest.{FunSpec, GivenWhenThen, Matchers}


class AccountsSpec extends FunSpec with Matchers with GivenWhenThen {

  describe("...") {
    it("...") {
      val m = account.deposit(account.create, nat.succ(nat.zero))
      val acc: ACCOUNTasBalance = m()
      acc.balance.state shouldEqual 1
    }
  }
}
