package chapter6

import org.scalatest.{FunSuite, Matchers}

class Chapter6Spec extends FunSuite with Matchers {

  test("RNG nonNegativeInt") {
    val rng = SimpleRNG(-2937659)
    rng.nextInt._1 should be < 0
    val (result1, rng1) = nonNegativeInt(rng)
    val result2 = nonNegativeInt(rng1)._1
    result1 should be >= 0
    result2 should be >= 0
    result1 should not be result2
  }

}
