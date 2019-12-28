package chapter4

import chapter4._
import org.scalatest.{FunSuite, Matchers}

class Chapter4Spec extends FunSuite with Matchers {
  test("map") {
    Some("hey").map(_ + " world") shouldEqual Some("hey world")
    val none: Option[Int] = None
    none.map(_ + 1) shouldEqual None
  }

  test("flatMap") {
    def lookup(v: String): Option[String] = v match {
      case "hey" => Some("world")
      case _ => None
    }

    Some("hey").flatMap(lookup) shouldEqual Some("world")
    Some("not hey").flatMap(lookup) shouldEqual None
    None.flatMap(lookup) shouldEqual None
  }

  test("getOrElse") {
    Some("hey").getOrElse("world") shouldEqual "hey"
    None.getOrElse("world") shouldEqual "world"
  }

  test("orElse") {
    Some("hey").orElse(Some("world")) shouldEqual Some("hey")
    Some("hey").orElse(None) shouldEqual Some("hey")
    None.orElse(Some("world")) shouldEqual Some("world")
    None.orElse(None) shouldEqual None
  }

  test("filter") {
    Some("hey").filter(_ == "hey") shouldEqual Some("hey")
    Some("not hey").filter(_ == "hey") shouldEqual None
    None.filter(_ == "hey") shouldEqual None
  }

  test("variance"){
    variance(Seq()) shouldEqual None
    variance(Seq(0.0, 0.0, 1.0, 1.0)) shouldEqual Some(0.25)
  }

  test("map2"){
    map2(Some(2), Some(3))(_ + _) shouldEqual Some(5)
    map2(Some(2), None)(_ + _) shouldEqual None
    map2(None: Option[Int], Some(2))(_ + _) shouldEqual None
    map2(None: Option[Int], None)(_ + _) shouldEqual None
  }

  test("sequence"){
    sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    sequence(List()) shouldEqual Some(List())
    sequence(List(Some(1), None)) shouldEqual None
    sequence(List(None, Some(1))) shouldEqual None
    sequence(List(None)) shouldEqual None
  }

  test("traverse"){
   traverse(List("1", "2", "3"))(i => if(i == "1" || i == "2" || i == "3") Some(i.toInt) else None) shouldEqual Some(List(1, 2, 3))
   traverse(List("1", "2", "3"))(i => if(i == "1") Some(i.toInt) else None) shouldEqual None
  }

  test("sequence2"){
    sequence2(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    sequence2(List()) shouldEqual Some(List())
    sequence2(List(Some(1), None)) shouldEqual None
    sequence2(List(None, Some(1))) shouldEqual None
    sequence2(List(None)) shouldEqual None
  }
}
