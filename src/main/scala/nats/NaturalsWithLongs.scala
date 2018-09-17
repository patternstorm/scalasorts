package nats

import universe.Universe._

trait NaturalsWithLongs extends Naturals {

  type NatAsLong = Long

  implicit object NatAsLong extends (nat as NatAsLong) {
    override def encode(x: nat.rep): NatAsLong = x match {
      case nat._zero => 0
      case nat._succ(n) => encode(n) + 1
    }

    override def decode(x: NatAsLong): nat.rep = if (x == 0) nat._zero else nat._succ(decode(x - 1))
  }

}
