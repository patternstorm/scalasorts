package sets

trait Sets {

  class `,`[A,B]
  implicit class Element[A](a: A) {
    def `,`[B](b: B): A `,` B = new `,`[A,B]{}
  }

  object Set {
    def `{`[A, B](elems: A `,` B): SBL[A, B] = SBL(elems)
  }

   case class SBL[A,B](elems: A `,` B ) {
      def `}`: Set[_] = ???
    }

  3 `,` 5 `,` "r"

}
