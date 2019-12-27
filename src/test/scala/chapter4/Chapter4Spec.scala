package chapter4

import org.scalatest.{FunSuite, Matchers}
import chapter4._

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
}