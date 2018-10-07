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







      implicitly[int.zero.type]
      val n = int.zero()
      val zeroRep: int.rep = n()
      zeroRep shouldEqual int.zero.rep

      val m = int.succ(int.zero())
      val succZeroRep: int.rep = m()
      succZeroRep shouldEqual int.succ.rep(int.zero.rep)

      val x = int.add(int.succ(int.zero()), int.zero())
      val rep: int.rep = x()
      rep shouldEqual int.succ.rep(int.zero.rep)

      val b = int.bool2Int(bool.`true`)
      val rep2: int.rep = b()
      rep2 shouldEqual int.succ.rep(int.zero.rep)


      val sort1 = implicitly[int.type]
      val sort2 = implicitly[int.sort]
      sort1 shouldEqual int.me
      sort1 shouldEqual sort2
      int.symbol shouldEqual "int"
    }
  }
}






