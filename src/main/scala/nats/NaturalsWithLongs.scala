package nats

import universe.Universe._

trait NaturalsWithLongs {
  self: Naturals =>

  type NatAsLong = Long

  implicit object NatAsLong extends (nat as NatAsLong) {
    override def encode(x: nat.rep): NatAsLong = x match {
      case nat.zero.rep => 0
      case nat.succ.rep(n) => encode(n) + 1
    }

    override def decode(x: NatAsLong): nat.rep = if (x == 0) nat.zero.rep else nat.succ.rep(decode(x - 1))
  }

}
