package universe

trait Arrows {
  self: Individuals =>

  trait Arrow extends Universal {
    type Domain <: Universal
    type Image <: Universal
    override type rep = Domain#rep => Image#rep
  }

  object Arrow {
    implicit def asArrow[X <: Universal , Y <: Universal]: X ->: Y = new Arrow { type Domain = X; type Image = Y }
  }

  type ->:[X <: Universal ,Y <: Universal] = Arrow { type Domain = X; type Image = Y }
}
