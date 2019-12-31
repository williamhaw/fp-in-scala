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

  test("RNG double") {
    val rng = SimpleRNG(42)
    val (double1, rng1) = double(rng)
    val double2 = double(rng1)._1

    double1.toInt should be >= 0
    double1.toInt should be < 1
    double2.toInt should be >= 0
    double2.toInt should be < 1
    double1 should not be double2
  }

  test("RNG doubleUsingMap") {
    val rng = SimpleRNG(42)
    val (double1, nextRng) = doubleUsingMap(rng)
    val double2 = doubleUsingMap(nextRng)._1

    double1.toInt should be >= 0
    double1.toInt should be < 1
    double2.toInt should be >= 0
    double2.toInt should be < 1
    double1 should not be double2
  }

  test("RNG map2") {
    val rng = SimpleRNG(42)
    val firstR = int(rng)
    val secondR = int(int(rng)._2)
    val (result, _) = map2(int, int)(_ + _)(rng)
    result shouldEqual firstR._1 + secondR._1
  }

  test("RNG sequence") {
    val rng = SimpleRNG(42)
    val firstR = int(rng)
    val secondR = int(int(rng)._2)
    val (result, _) = sequence(List(int, int))(rng)
    result shouldEqual List(firstR._1, secondR._1)
  }
}
