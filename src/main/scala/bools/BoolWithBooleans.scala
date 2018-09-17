package bools

import universe.Universe._

import scala.language.implicitConversions


trait BoolWithBooleans {
  self: Bools =>

  case class BOOLasBoolean(state: Boolean)

  implicit object BOOLasBoolean extends (bool as BOOLasBoolean) {
    override def encode(x: bool.rep): BOOLasBoolean = x match {
      case bool.`true`.rep => BOOLasBoolean(true)
      case bool.`false`.rep => BOOLasBoolean(false)
    }

    override def decode(x: BOOLasBoolean): bool.rep = if (x.state) bool.`true`.rep else bool.`false`.rep
  }


}
