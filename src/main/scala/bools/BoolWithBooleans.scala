package bools

import universe.Universe._

import scala.language.implicitConversions


trait BoolWithBooleans extends Bools {

  case class BOOLasBoolean(state: Boolean)

  implicit object BOOLasBoolean extends (bool as BOOLasBoolean) {
    override def encode(x: bool.rep): BOOLasBoolean = x match {
      case bool._true => BOOLasBoolean(true)
      case bool._false => BOOLasBoolean(false)
    }

    override def decode(x: BOOLasBoolean): bool.rep = if (x.state) bool._true else bool._false
  }


}
