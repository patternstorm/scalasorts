package universe

trait Nothing {
  self: Individuals =>
  type nothing = nothing.type
  implicit object nothing extends Universal {
    override type rep = Unit
  }
}
