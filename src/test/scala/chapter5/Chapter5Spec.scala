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
    Stream(2, 3, 4, 5).takeWhile(_ % 2 == 0).toList shouldEqual List(2)
    Stream(2, 4, 5).takeWhile(_ % 2 == 0).toList shouldEqual List(2, 4)
    Stream(1, 3, 5).takeWhile(_ % 2 == 0).toList shouldEqual List.empty
    Stream[Int]().takeWhile(_ % 2 == 0).toList shouldEqual List.empty
  }

  test("Stream forall") {
    Stream(2, 4, 6, 8, 10).forall(_ % 2 == 0) shouldEqual true
    Stream(2, 4, 6, 8, 11).forall(_ % 2 == 0) shouldEqual false
    Stream[Int]().forall(_ % 2 == 0) shouldEqual true
  }

  test("Stream takeWhileWithFoldRight") {
    Stream(2, 3, 4, 5).takeWhileWithFoldRight(_ % 2 == 0).toList shouldEqual List(2)
    Stream(2, 4, 5).takeWhileWithFoldRight(_ % 2 == 0).toList shouldEqual List(2, 4)
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
    Stream(1, 2, 3).flatMap(i => Stream(i + 1, i + 2)).toList shouldEqual List(2, 3, 3, 4, 4, 5)
    Stream[Int]().flatMap(i => Stream(i + 1, i + 2)).toList shouldEqual List.empty
  }

  test("Stream constant") {
    constant(1).take(3).toList shouldEqual List(1, 1, 1)
    constant("hello").take(3).toList shouldEqual List("hello", "hello", "hello")
  }

  test("Stream from") {
    from(1).take(3).toList shouldEqual List(1, 2, 3)
    from(17).take(3).toList shouldEqual List(17, 18, 19)
  }

  test("Stream fibs") {
    fibs.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  test("Stream unfold") {
    unfold(1)(s => Some(s, s + 1)).take(3).toList shouldEqual List(1, 2, 3)
  }

  test("Stream fibsUsingUnfold") {
    fibsUsingUnfold.take(7).toList shouldEqual List(0, 1, 1, 2, 3, 5, 8)
  }

  test("Stream fromUsingUnfold") {
    fromUsingUnfold(1).take(3).toList shouldEqual List(1, 2, 3)
    fromUsingUnfold(17).take(3).toList shouldEqual List(17, 18, 19)
  }

  test("Stream constantUsingUnfold") {
    constantUsingUnfold(1).take(3).toList shouldEqual List(1, 1, 1)
    constantUsingUnfold("hello").take(3).toList shouldEqual List("hello", "hello", "hello")
  }

  test("Stream onesUsingUnfold") {
    onesUsingUnfold.take(3).toList shouldEqual List(1, 1, 1)
  }

  test("Stream mapUsingUnfold") {
    Stream(1, 2, 3).mapUsingUnfold(_.toString).toList shouldEqual List("1", "2", "3")
    Stream[Int]().mapUsingUnfold(_.toString).toList shouldEqual List.empty
  }

  test("Stream takeUsingUnfold") {
    Stream(1, 2, 3, 4).takeUsingUnfold(0).toList shouldEqual List.empty
    Stream(1, 2, 3, 4).takeUsingUnfold(2).toList shouldEqual List(1, 2)
    Stream(1, 2, 3, 4).takeUsingUnfold(10).toList shouldEqual List(1, 2, 3, 4)
  }

  test("Stream takeWhileUsingUnfold") {
    Stream(2, 3, 4, 5).takeWhileUsingUnfold(_ % 2 == 0).toList shouldEqual List(2)
    Stream(2, 4, 5).takeWhileUsingUnfold(_ % 2 == 0).toList shouldEqual List(2, 4)
    Stream(1, 3, 5).takeWhileUsingUnfold(_ % 2 == 0).toList shouldEqual List.empty
    Stream[Int]().takeWhileUsingUnfold(_ % 2 == 0).toList shouldEqual List.empty
  }

  test("Stream zipWith") {
    Stream("a", "b", "c").zipWith(Stream("a", "b"))((a, b) => a + b).toList shouldBe List("aa", "bb")
  }

  test("Stream zipAll") {
    Stream("a", "b", "c").zipAll(Stream("a", "b")).toList shouldEqual List((Some("a"), Some("a")), (Some("b"), Some("b")), (Some("c"), None))
  }

  test("Stream startswith") {
    Stream("a", "b", "c").startsWith(Stream("a", "b")) shouldEqual true
    Stream("a", "b", "c").startsWith(Stream("aa")) shouldEqual false
    Stream("a", "b", "c").startsWith(Stream("a", "bb")) shouldEqual false
    Stream(1, 2, 3).startsWith(Stream(1, 2)) shouldEqual true
    Stream().startsWith(Stream()) shouldEqual true
  }

  test("Stream tails") {
    Stream(1, 2, 3).tails.toList.map(_.toList) shouldEqual List(List(1, 2, 3), List(2, 3), List(3))
    Stream("a", "b", "c").tails.toList.map(_.toList) shouldEqual List(List("a", "b", "c"), List("b", "c"), List("c"))
  }

  test("Stream scanRight") {
    Stream(1,2,3).scanRight(0)(_ + _).toList shouldEqual List(6, 5, 3, 0)
  }
}
