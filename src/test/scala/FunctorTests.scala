package tests

import org.scalatest.FunSuite
import pointwise_composable_functors.SeqF
import pointwise_composable_functors.SeqF._


class FunctorTests extends FunSuite {
  test("Test non-pointwise map composure") {
    val v1 = (1 to 5).wrapf.map(x => x+1).map(x => x*2).values
    val t1 = (1 to 5).map(x => x+1).map(x => x*2)
    assert(v1 == t1)
  }

  test("Test that intermediate results of a fold are correct") {
    val tmp1 = (1 to 5).wrapf.map(x => x + 1).map(x => x*2).scanLeft(0)(_+_)
    val v1 = tmp1.values()
    val t1 = Vector(0, 4, 10, 18, 28, 40)
    assert(v1 == t1)
  }
}
