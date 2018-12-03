package nats

import universe.Universe._
import nats.NAT._


object NATasLong extends nat.impl {

  case class rep(value: Long) extends AnyVal {
    override def toString: String = symbol + "(value=" + value + ")"
  }

  override val symbol: String = "NATasLong"
  implicit val me: NATasLong.type = this

  implicit def unlift(a: Rep[NATasLong.type]): rep = a.value

  implicit def asLong(x: rep): Long = x.value

  implicit def asNatAsLong(x: Long): rep = NATasLong(x)

  override def _encode(x: nat.zero.rep): rep = 0

  override def _encode(x: nat.succ.rep): rep = asImp(x._1) + 1

  override def _decode(x: rep): nat.rep = if (x.value == 0) nat.zero.rep else nat.succ.rep(_decode(x - 1))

  def apply(value: Long): rep = rep(value)
}

object NATasLongImp extends nat.add.impl[NATasLong.type, NATasLong.type, NATasLong.type] {
  override def add(a: NATasLong.rep, b: NATasLong.rep): NATasLong.rep = NATasLong(a.value + b.value)
}
