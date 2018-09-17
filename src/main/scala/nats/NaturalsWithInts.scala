package nats

import universe.Universe._

trait NaturalsWithInts {
  self: Naturals =>

  case class NatAsInt(state: Int)

  implicit object NatAsInt extends (nat as NatAsInt) {
    implicit override def encode(x: nat.rep): NatAsInt = x match {
      case nat._zero => NatAsInt(0)
      case nat._succ(n) => NatAsInt(encode(n).state + 1)
    }

    implicit override def decode(x: NatAsInt): nat.rep = if (x.state == 0) nat._zero else nat._succ(decode(NatAsInt(x.state - 1)))
  }
}
