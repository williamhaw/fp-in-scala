package chapter5

import org.scalatest.{FunSuite, Matchers}

class Chapter5Spec extends FunSuite with Matchers {

  test("Stream toList") {
    Stream(1, 2, 3).toList shouldEqual List(1, 2, 3)
  }

  test("Stream take") {
    Stream(1, 2, 3, 4).take(0).toList shouldEqual List.empty
    Stream(1, 2, 3, 4).take(2).toList shouldEqual List(1, 2)
    Stream(1, 2, 3, 4).take(10).toList shouldEqual List(1, 2, 3, 4)
  }

  test("Stream drop") {
    Stream(1, 2, 3, 4).drop(0).toList shouldEqual List(1, 2, 3, 4)
    Stream(1, 2, 3, 4).drop(2).toList shouldEqual List(3, 4)
    Stream(1, 2, 3, 4).drop(10).toList shouldEqual List.empty
  }

  test("Stream takeWhile") {
    Stream(1, 2, 3, 4, 5).takeWhile(_ % 2 == 0).toList shouldEqual List(2, 4)
    Stream(1, 3, 5).takeWhile(_ % 2 == 0).toList shouldEqual List.empty
    Stream[Int]().takeWhile(_ % 2 == 0).toList shouldEqual List.empty
  }
}
