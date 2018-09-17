package banking.accounts

import nats.Naturals
import universe.Universe._


trait Accounts {
  self: Naturals =>

  type account = account.type

  implicit object account extends Sort {


    sealed trait rep

    //create: -> account
    type create = create.type

    implicit case object create extends Operator {
      def apply()(implicit m: create :: account): create :: account = m

      implicit object imp extends (create :: account) {
        override def apply(): account.rep = rep
      }

      case object rep extends account.rep

    }

    //deposit: account -> int -> account
    type deposit = deposit.type

    implicit object deposit extends Operator {
      def apply[N <: Particular, A <: Particular](account: A :: account, amount: N :: nat)(implicit m: ((deposit ∙ A) ∙ N) :: account): ((deposit ∙ A) ∙ N) :: account = m

      implicit object imp extends (deposit :: account ->: nat ->: account) {
        override def apply(): account.rep => nat.rep => account.rep = acc => n => rep(acc, n)
      }

      case class rep(acc: account.rep, amount: nat.rep) extends account.rep

    }

  }

}
