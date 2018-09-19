package banking.accounts

import nats.Naturals
import universe.Universe.{::, ∙, _}


trait Accounts {
  self: Naturals =>

  type account = account.type

  implicit object account extends Sort {


    sealed trait rep

    //create: -> account
    type create = create.type

    implicit case object create extends Operator {
      def apply(): create = create

      implicit object imp extends (create :: account) {
        override def apply(): account.rep = rep
      }

      case object rep extends account.rep

    }

    //deposit: account -> int -> account
    type deposit = deposit.type

    implicit object deposit extends Operator {
      def apply[N <: Particular, A <: Particular](account: A, amount: N)(implicit ev1: A :: account, ev2: N :: nat, m: (deposit ∙ A) ∙ N): (deposit ∙ A) ∙ N = m

      implicit object imp extends (deposit :: account ->: nat ->: account) {
        override def apply(): account.rep => nat.rep => account.rep = acc => n => rep(acc, n)
      }

      case class rep(acc: account.rep, amount: nat.rep) extends account.rep

    }

  }

}
