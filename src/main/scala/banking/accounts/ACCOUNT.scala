package banking.accounts

import dsl.Statements._
import nats.NAT._
import universe.Universe._


object ACCOUNT {

  @sort trait account {
    val acc: account;
    val n: nat

    def create: () => account

    def deposit: (account, nat) => account

    def balance: account => nat = {
      create() -> nat.zero()
      deposit(acc, n) -> nat.add(balance(acc), n)
    }
  }


  //  object account extends Sort {
  //    type sort = account.type
  //
  //    implicit def me: sort = this
  //
  //    override val symbol = "account"
  //
  //    sealed trait rep
  //
  //    implicit object create extends Operator {
  //      override type self = create.type
  //      override val symbol: String = "create"
  //
  //      implicit object imp extends ::[create.type, account.type] {
  //        override def apply(): account.type#rep = rep
  //      }
  //
  //      type rep = rep.type
  //      //def apply()(implicit x: self): self = x
  //
  //      case object rep extends account.type#rep
  //      implicit def asRep(x: create.type): account.type#rep = rep
  //
  //      trait impl[A <: Representation] {
  //        def create(): A
  //
  //        implicit object createImp extends impAs[account.create.type, A] {
  //          override def apply(): A = create()
  //        }
  //      }
  //
  //    }
  //
  //    implicit object deposit extends Operator {
  //      override type self = deposit.type
  //      override val symbol: String = "deposit"
  //
  //      implicit object imp extends ::[deposit.type, account.type ->: nat.type ->: account.type] {
  //        override def apply(): account.type#rep => nat.type#rep => account.type#rep = x => y => rep(x, y)
  //      }
  //
  //      case class rep(_1: account.type#rep, _2: nat.type#rep) extends account.rep
  //
  //      def apply[X <: Particular, Y <: Particular](x: X, y: Y)(implicit ev1: X :: account.type, ev2: Y:: nat.type, m: deposit.type ∙ X ∙ Y): deposit.type ∙ X ∙ Y = m
  //
  //      trait impl[A <: Representation, B <: Representation, C <: Representation] {
  //        def deposit(account: A, amount: B): C
  //
  //        implicit object depositImp extends impAs[self, (A => B => C)] {
  //          override def apply(): A => B => C = a => b => deposit(a, b)
  //        }
  //
  //      }
  //
  //    }
  //
  //    implicit object balance extends Operator {
  //      override type self = balance.type
  //      override val symbol: String = "balance"
  //
  //      implicit object imp extends ::[balance.type, account.type ->: nat.type] {
  //        override def apply(): account.type#rep => nat.type#rep = _1 => _1 match {
  //          case `create`.rep =>
  //            nat.zero.imp()
  //          case `deposit`.rep(acc, n) =>
  //            nat.add.imp()(this ()(acc))(n)
  //        }
  //      }
  //
  //      def apply[Y <: Particular](x: Y)(implicit ev: Y :: account.type, m: balance.type ∙ Y): balance.type ∙ Y = m
  //
  //      trait impl[A <: Representation, B <: Representation] {
  //        def balance(account: A): B
  //
  //        implicit object balanceImp extends impAs[self, (A => B )] {
  //          override def apply(): A => B = a => balance(a)
  //        }
  //
  //      }
  //    }
  //
  //    trait as[T <: Representation] {
  //      protected def _decode(x: T): account.type#rep
  //
  //      implicit def asImp(x: account.type#rep): T = x match {
  //        case t: create.type#rep =>
  //          _encode(t): T
  //        case t: deposit.type#rep =>
  //          _encode(t): T
  //      }
  //
  //      implicit def asRep(x: T): account.type#rep = _decode(x)
  //
  //      implicit object imp extends reps[T,account.type] {
  //        override def encode(x: account.type#rep): T = asImp(x)
  //        override def decode(x: T): account.type#rep = ??? //asRep(x)
  //      }
  //
  //      protected def _encode(x: create.type#rep): T
  //      protected def _encode(x: deposit.type#rep): T
  //    }
  //
  //  }

}
