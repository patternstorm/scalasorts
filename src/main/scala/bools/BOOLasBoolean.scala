package bools

import universe.Universe._
import bools.BOOL._


object BOOLasBoolean extends bool.impl {

  case class rep(state: Boolean) {
    override def toString: String = symbol + "(state=" + state + ")"
  }

  override val symbol: String = "BOOLasBoolean"
  implicit val me: BOOLasBoolean.type = this

  implicit def unlift(a: Rep[BOOLasBoolean.type]): rep = a.value

  override protected def _encode(x: bool.`true`.rep): rep = BOOLasBoolean(true)

  override protected def _encode(x: bool.`false`.rep): rep = BOOLasBoolean(false)

  override protected def _decode(x: rep): bool.rep = if (x.state) bool.`true`.rep else bool.`false`.rep

  def apply(state: Boolean): rep = rep(state)
}

object BOOLasBooleanImp extends bool.not.impl[BOOLasBoolean.type, BOOLasBoolean.type] {
  override def not(a: BOOLasBoolean.rep): BOOLasBoolean.rep = BOOLasBoolean(!a.state)
}

