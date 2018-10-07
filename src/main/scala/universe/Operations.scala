package universe


// TODO: syntactic sugar traits to declare ops + inductive types to classify ops in arity, constructor/modifier/query, etc.

trait Operations {
  self: Individuals with Particulars with Universals =>

  //f A -> B
  //  abstract class Operation2[A <: Universal, B <: Universal](name: String) {
  //    type $name = $name.type
  //
  //    implicit object $name extends Operator {
  //      override val symbol = name
  //
  //      def apply[X <: Particular](x: X)(implicit ev: X :: A, m: $name ∙ X): $name ∙ X = m
  //
  //      implicit object imp extends ($name :: A ->: B) {
  //        override def apply(): A#rep => B#rep = x => rep(x)
  //      }
  //
  //      case class rep(n: A#rep) extends B#rep
  //
  //    }
  //
  //  }


  //}


}

