package ints

import universe.Universe._

trait IntegersWithLongs {
  self: Integers =>

  type IntAsLong = Long

  implicit object NatAsLong extends (int as IntAsLong) {
    override def encode(x: Rep[int]): IntAsLong = x match {
      case int._zero => 0
      case int._succ(n) => encode(n) + 1
    }

    override def decode(x: IntAsLong): Rep[int] = if (x == 0) int._zero else int._succ(decode(x - 1))
  }

}
