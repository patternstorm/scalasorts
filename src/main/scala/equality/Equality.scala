package equality

import universe.Universe.Individual

trait Equality {

  type ⟿[X <: Individual, Y <: Individual] = Equals[X, Y]

  trait Equals[X <: Individual, Y <: Individual]

  object Equals {
    implicit def reflexivity[X <: Individual]: Equals[X, X] = new Equals[X, X] {}

    implicit def transitivity[X <: Individual, Y <: Individual, Z <: Individual]
    (implicit ev1: Equals[X, Y], eb2: Equals[Y, Z]): Equals[X, Z] = new Equals[X, Z] {}
  }

}
