package ints

import universe.Universe._

trait IntegersWithInts {
  self: Integers =>

  case class IntAsInt(state: Int)

  implicit object IntAsInt extends (int as IntAsInt) {
    override def encode(x: int.rep): IntAsInt = x match {
      case int.zero.rep => IntAsInt(0)
      case int.succ.rep(n) => IntAsInt(encode(n).state + 1)
    }

    override def decode(x: IntAsInt): int.rep = if (x.state == 0) int.zero.rep else int.succ.rep(decode(IntAsInt(x.state - 1)))
  }

}

