package tests

import org.scalatest.FunSuite
import scala.concurrent._
import scala.concurrent.duration._
import applicative.ApplicativeSeq
import applicative.ApplicativeSeq._


class ApplicativeFunctorTests extends FunSuite {
  test("Non-pointwise map composure") {
    val v1 = (1 to 5).wrapf.map(x => x+1).map(x => x*2).values
    val t1 = (1 to 5).map(x => x+1).map(x => x*2)
    assert(v1 == t1)
  }

  test("That intermediate results of a fold are correct") {
    val tmp1 = (1 to 5).wrapf.map(x => x + 1).map(x => x*2).scanLeft(0)(_+_)
    val v1 = tmp1.values()
    val t1 = Vector(0, 4, 10, 18, 28, 40)
    assert(v1 == t1)
  }

  test("Pointwise composure") {
    var t1 = ""
    (1 to 2).wrapf.map(x => t1 += "f").map(x => t1 += "g").values
    assert(t1 == "fgfg")
    var t2 = ""
    (1 to 2).map(x => t2 += "f").map(x => t2 += "g")
    assert(t2 == "ffgg")
  }
}
