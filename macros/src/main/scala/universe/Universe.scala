package universe

trait Implicits {
  self: Universals =>
  implicit def imp0[A](x: A): () => A = ???

  implicit def imp1[A, B](x: (A, B)): A => B = ???

  implicit def imp2[A, B, C](x: ((A, B), C)): (A, B) => C = ???

  implicit def repAsSort[U <: Sort](x: U#rep): U = ???
}

trait Entities extends Individuals with Particulars with Universals with Morphisms with Equality with Implicits

object Universe extends Entities {

}







