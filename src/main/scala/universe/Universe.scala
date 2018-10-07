package universe

import equality.Equality


object Universe extends Individuals with Particulars with Universals with Operations with Morphisms with Equality {
  implicit def imp0[A](x: A): () => A = ???

  implicit def imp1[A, B](x: (A, B)): A => B = ???

  implicit def imp2[A, B, C](x: ((A, B), C)): (A, B) => C = ???

  implicit def repAsSort[U <: Sort](x: U#rep): U = ???
}







