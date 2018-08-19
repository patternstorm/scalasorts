package universe

trait Nothing {
  self: Individuals =>
  type Nothing = Nothing.type
  implicit object Nothing extends Universal {
    override type Rep = Unit
  }
}
