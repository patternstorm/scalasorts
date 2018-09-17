package banking.accounts

import nats.Naturals
import universe.Universe._


trait Accounts {
  self: Naturals =>

  type account = account.type

  implicit object account extends Sort {

    override type rep = Rep[account]

    sealed trait _account extends rep

    case object _create extends _account

    case class _deposit(acc: rep, amount: nat.rep) extends _account

    //create: -> account
    type create = create.type

    implicit object create extends Operator {
      def apply()(implicit m: create :: account): create :: account = m

      implicit object imp extends (create :: account) {
        override def apply(): rep = _create
      }

    }

    //deposit: account -> int -> account
    type deposit = deposit.type

    implicit object deposit extends Operator {
      def apply[N <: Particular, A <: Particular](account: A :: account, amount: N :: nat)
                                                 (implicit m: ((deposit ∙ A) ∙ N) :: account): ((deposit ∙ A) ∙ N) :: account = m

      implicit object imp extends (deposit :: account ->: nat ->: account) {
        override def apply(): rep => nat.rep => rep = acc => n => _deposit(acc, n)
      }

    }

  }

}
