package chapter3

import org.scalatest.{FunSuite, Matchers}

class Chapter3Spec extends FunSuite with Matchers{

  test("testTail") {
    tail(List(1, 2, 3)) shouldBe List(2,3)
    tail(List(1)) shouldBe Nil
  }

  test("testSetHead") {
    setHead(3, List(1, 2, 3)) shouldBe List(3, 2, 3)
    setHead("c", List("a", "b")) shouldBe List("c", "b")
  }

  test("testDrop") {
    drop(List(1, 2, 3), 1) shouldBe List(2, 3)
    drop(List(1, 2, 3), 0) shouldBe List(1, 2, 3)
    drop(List("a", "b"), 2) shouldBe List()
    drop(List(1, 2), 3) shouldBe List()
    drop(Nil, 1) shouldBe Nil
  }

  test("testDropWhile") {
    dropWhile(List(1, 2, 3), (x: Int) => x < 2) shouldBe List(2, 3)
    dropWhile(List(1, 2, 3), (x: Int) => x > 2) shouldBe List(1, 2, 3) //returns whole list because loop stops the first time f is false
    dropWhile(List(1, 2, 3), (x: Int) => x > 0) shouldBe List()
    dropWhile(Nil, (x: Int) => x > 0) shouldBe Nil
  }

  test("testInit"){
    init(List(1, 2, 3)) shouldBe List(1, 2)
    init(List(1)) shouldBe Nil
  }

  test("testLength"){
    chapter3.length(List(1, 2, 3, 4, 5)) shouldBe 5
  }

  test("test3.11"){
    def listInts = List(1, 2, 3, 4, 5)
    def listDoubles = List(1.0, 2.0, 3.0)
    sum3(listInts) shouldBe 15
    product3(listDoubles) shouldBe 6.0
    length2(listInts) shouldBe 5
  }

  test("testReverse"){
    reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
  }

  test("testAppend"){
    append(List(1, 2, 3), List(1, 2)) shouldBe List(1, 2, 3, 1, 2)
    append(List(1, 2, 3), Nil) shouldBe List(1, 2, 3)
    append(Nil, List(1, 2)) shouldBe List(1, 2)
    append(Nil, Nil) shouldBe Nil
  }

  test("testConcat"){
    concat(List(List(1, 2, 3), List(1, 2, 3), List(1, 2, 3))) shouldBe List(1, 2, 3, 1, 2, 3, 1, 2, 3)
    concat(List(List(1, 2, 3), Nil, List(4, 5, 6))) shouldBe List(1, 2, 3, 4, 5, 6)
    concat(List(Nil, Nil, Nil)) shouldBe Nil
  }

  test("test adding 1 to each element of list"){
    add1(List(1, 2, 3)) shouldBe List(2, 3, 4)
  }

  test("test transforming List[Double] to List[String]"){
    doubleListToString(List(1.0, 2.0, 3.0)) shouldBe List("1.0", "2.0", "3.0")
  }

  test("test map for addition and toString"){
    map(List(1, 2, 3))(x => x + 1) shouldBe List(2, 3, 4)
    map(List(1.0, 2.0, 3.0))(x => x.toString) shouldBe List("1.0", "2.0", "3.0")
  }

  test("filter odd numbers out"){
    filter(List(1, 2, 3, 4))(x => x % 2 == 0) shouldBe List(2, 4)
  }

  test("flatmap"){
    flatMap(List(1, 2, 3))(x => List(x, x)) shouldBe List(1, 1, 2, 2, 3, 3)
  }

  test("filter using flatmap"){
    filterUsingFlatmap(List(1, 2, 3, 4))(x => x % 2 == 0) shouldBe List(2, 4)
  }
}
