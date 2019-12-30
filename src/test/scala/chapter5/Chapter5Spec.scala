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

  test("Stream forall") {
    Stream(2, 4, 6, 8, 10).forall(_ % 2 == 0) shouldEqual true
    Stream(2, 4, 6, 8, 11).forall(_ % 2 == 0) shouldEqual false
    Stream[Int]().forall(_ % 2 == 0) shouldEqual true
  }

  test("Stream takeWhileWithFoldRight") {
    Stream(1, 2, 3, 4, 5).takeWhileWithFoldRight(_ % 2 == 0).toList shouldEqual List(2, 4)
    Stream(1, 3, 5).takeWhileWithFoldRight(_ % 2 == 0).toList shouldEqual List.empty
    Stream[Int]().takeWhileWithFoldRight(_ % 2 == 0).toList shouldEqual List.empty
  }


  test("Stream headOptionWithFoldRight") {
    Stream(1).headOptionWithFoldRight shouldEqual Some(1)
    Stream[Int]().headOptionWithFoldRight shouldEqual None
  }

  test("Stream map") {
    Stream(1, 2, 3).map(_.toString).toList shouldEqual List("1", "2", "3")
    Stream[Int]().map(_.toString).toList shouldEqual List.empty
  }

  test("Stream filter") {
    Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList shouldEqual List(2, 4)
    Stream(2, 4).filter(_ % 2 == 0).toList shouldEqual List(2, 4)
    Stream(1, 3).filter(_ % 2 == 0).toList shouldEqual List.empty
    Stream[Int]().filter(_ % 2 == 0).toList shouldEqual List.empty
  }

  test("Stream append") {
    Stream(1).append(Stream(2)).toList shouldEqual List(1, 2)
    Stream[Int]().append(Stream(1)).toList shouldEqual List(1)
    Stream(1).append(Stream[Int]()).toList shouldEqual List(1)
  }

  test("Stream flatmap") {
    Stream(1, 2, 3).flatMap( i => Stream(i + 1, i + 2)).toList shouldEqual List(2, 3, 3, 4, 4, 5)
    Stream[Int]().flatMap( i => Stream(i + 1, i + 2)).toList shouldEqual List.empty
  }

  test("Stream constant") {
    constant(1).take(3).toList shouldEqual List(1, 1, 1)
    constant("hello").take(3).toList shouldEqual List("hello", "hello", "hello")
  }

  test("Stream from") {
    from(1).take(3).toList shouldEqual List(1, 2, 3)
    from(17).take(3).toList shouldEqual List(17, 18, 19)
  }
}
