package ints

import dsl.Statements._
import universe.Universe._

object INT {

  @sort trait int {
    var x, y: int

    def zero: () => int

    def succ: int => int

    def add: (int, int) => int = {
      (zero(), x) -> x
      (succ(x), y) -> succ(add(x, y))
    }
  }

}