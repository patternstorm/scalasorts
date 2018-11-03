package bools

import universe.Universe._
import bools.BOOL._

case class BOOLasBoolean(state: Boolean)

object BOOLasBoolean extends bool.imp[BOOLasBoolean] {

  override protected def _encode(x: bool.`true`.rep): BOOLasBoolean = BOOLasBoolean(true)

  override protected def _encode(x: bool.`false`.rep): BOOLasBoolean = BOOLasBoolean(false)

  override protected def _decode(x: BOOLasBoolean): bool.rep = if (x.state) bool.`true`.rep else bool.`false`.rep

  implicit object notAsBOOLasBoolean extends Implementation[bool.not.type, bool.sort ->: bool.sort, BOOLasBoolean => BOOLasBoolean] {
    override def apply(): BOOLasBoolean => BOOLasBoolean = x => BOOLasBoolean(!x.state)
  }

}

