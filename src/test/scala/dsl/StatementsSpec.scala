package dsl

import dsl.Statements._
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}
import universe.Universe._
import bools.BOOL._

class StatementsSpec extends FunSpec with Matchers with GivenWhenThen {


  describe("..") {
    it("..") {


      @sort trait int {
        type bool = bool.sort
        var x, y: int

        def zero: () => int

        def succ: int => int

        def add: (int, int) => int = {
          (zero(), x) -> x
          (succ(x), y) -> succ(add(x, y))
        }

        def bool2Int: bool => int = {
          bool.`true`() -> succ(zero())
          bool.`false`() -> zero()
        }

      }

      implicit object canonicZero extends ::[int.zero.type, int.type] {
        override def apply(): int.rep = int.zero.rep
      }







      implicitly[int.zero.type]


      val succZeroRep: int.rep = int.succ(int.zero())
      succZeroRep shouldEqual int.succ.rep(int.zero.rep)

      var rep: int.rep = int.add(int.succ(int.zero()), int.zero())
      rep shouldEqual int.succ.rep(int.zero.rep)

      rep = int.bool2Int(bool.`true`)
      rep shouldEqual int.succ.rep(int.zero.rep)


      val sort1 = implicitly[int.type]
      val sort2 = implicitly[int.sort]
      sort1 shouldEqual int.me
      sort1 shouldEqual sort2
      int.symbol shouldEqual "int"
    }
  }
}






