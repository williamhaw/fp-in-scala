package chapter5

import org.scalatest.{FunSuite, Matchers}

class Chapter5Spec extends FunSuite with Matchers {

  test("Stream toList") {
    Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
  }
}
