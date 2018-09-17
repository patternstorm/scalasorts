package ints

import universe.Universe._

trait IntegersWithLongs {
  self: Integers =>

  type IntAsLong = Long

  implicit object NatAsLong extends (int as IntAsLong) {
    override def encode(x: int.rep): IntAsLong = x match {
      case int.zero.rep => 0
      case int.succ.rep(n) => encode(n) + 1
    }

    override def decode(x: IntAsLong): int.rep = if (x == 0) int.zero.rep else int.succ.rep(decode(x - 1))
  }

}
