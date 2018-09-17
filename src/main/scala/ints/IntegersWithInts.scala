package ints

import universe.Universe._

trait IntegersWithInts {
  self: Integers =>

  case class IntAsInt(state: Int)

  implicit object IntAsInt extends (int as IntAsInt) {
    implicit override def encode(x: int.rep): IntAsInt = x match {
      case int._zero => IntAsInt(0)
      case int._succ(n) => IntAsInt(encode(n).state + 1)
    }

    implicit override def decode(x: IntAsInt): int.rep = if (x.state == 0) int._zero else int._succ(decode(IntAsInt(x.state - 1)))
  }

}

