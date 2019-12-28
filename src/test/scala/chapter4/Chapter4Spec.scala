package chapter4

import org.scalatest.{FunSuite, Matchers}

import scala.util.{Failure, Success, Try}

class Chapter4Spec extends FunSuite with Matchers {
  test("Option map") {
    Some("hey").map(_ + " world") shouldEqual Some("hey world")
    val none: Option[Int] = None
    none.map(_ + 1) shouldEqual None
  }

  test("Option flatMap") {
    def lookup(v: String): Option[String] = v match {
      case "hey" => Some("world")
      case _ => None
    }

    Some("hey").flatMap(lookup) shouldEqual Some("world")
    Some("not hey").flatMap(lookup) shouldEqual None
    None.flatMap(lookup) shouldEqual None
  }

  test("Option getOrElse") {
    Some("hey").getOrElse("world") shouldEqual "hey"
    None.getOrElse("world") shouldEqual "world"
  }

  test("Option orElse") {
    Some("hey").orElse(Some("world")) shouldEqual Some("hey")
    Some("hey").orElse(None) shouldEqual Some("hey")
    None.orElse(Some("world")) shouldEqual Some("world")
    None.orElse(None) shouldEqual None
  }

  test("Option filter") {
    Some("hey").filter(_ == "hey") shouldEqual Some("hey")
    Some("not hey").filter(_ == "hey") shouldEqual None
    None.filter(_ == "hey") shouldEqual None
  }

  test("Option variance") {
    variance(Seq()) shouldEqual None
    variance(Seq(0.0, 0.0, 1.0, 1.0)) shouldEqual Some(0.25)
  }

  test("Option map2") {
    map2(Some(2), Some(3))(_ + _) shouldEqual Some(5)
    map2(Some(2), None)(_ + _) shouldEqual None
    map2(None: Option[Int], Some(2))(_ + _) shouldEqual None
    map2(None: Option[Int], None)(_ + _) shouldEqual None
  }

  test("Option sequence") {
    sequence(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    sequence(List()) shouldEqual Some(List())
    sequence(List(Some(1), None)) shouldEqual None
    sequence(List(None, Some(1))) shouldEqual None
    sequence(List(None)) shouldEqual None
  }

  test("Option traverse") {
    traverse(List("1", "2", "3"))(i => if (i == "1" || i == "2" || i == "3") Some(i.toInt) else None) shouldEqual Some(List(1, 2, 3))
    traverse(List("1", "2", "3"))(i => if (i == "1") Some(i.toInt) else None) shouldEqual None
  }

  test("Option sequence2") {
    sequence2(List(Some(1), Some(2), Some(3))) shouldEqual Some(List(1, 2, 3))
    sequence2(List()) shouldEqual Some(List())
    sequence2(List(Some(1), None)) shouldEqual None
    sequence2(List(None, Some(1))) shouldEqual None
    sequence2(List(None)) shouldEqual None
  }

  test("Either map") {
    val error: Either[String, Int] = Left("error")
    val success: Either[String, Int] = Right(2)
    error.map(_ + 1) shouldEqual Left("error")
    success.map(_ + 1) shouldEqual Right(3)
  }

  test("Either flatMap") {
    val error: Either[String, Int] = Left("error")
    val success1: Either[String, Int] = Right(1)
    val success2: Either[String, Int] = Right(2)
    error.flatMap { case 1 => Left("My god! A 1!") case v => Right(v + 1) } shouldEqual Left("error")
    success1.flatMap { case 1 => Left("My god! A 1!") case v => Right(v + 1) } shouldEqual Left("My god! A 1!")
    success2.flatMap { case 1 => Left("My god! A 1!") case v => Right(v + 1) } shouldEqual Right(3)
  }

  test("Either orElse") {
    val error: Either[String, Int] = Left("error")
    val success: Either[String, Int] = Right(1)
    error.orElse(Right(2)) shouldEqual Right(2)
    success.orElse(Right(2)) shouldEqual Right(1)
  }

  test("Either map2") {
    val error: Either[String, Int] = Left("error")
    val success: Either[String, Int] = Right(1)
    error.map2(success)(_ + _) shouldEqual Left("error")
    success.map2(success)(_ + _) shouldEqual Right(2)
  }

  test("Either traverse") {
    traverseEither(List("1", "error1", "error2"))(v => Try {
      v.toInt
    } match {
      case Success(v) => Right(v)
      case Failure(e) => Left(e.getMessage)
    }) shouldEqual Left("For input string: \"error1\"")

    traverseEither(List("1", "2", "3"))(v => Try {
      v.toInt
    } match {
      case Success(v) => Right(v)
      case Failure(e) => Left(e.getMessage)
    }) shouldEqual Right(List(1, 2, 3))
  }

  test("Either sequence") {
    val error1: Either[String, Int] = Left("error1")
    val error2: Either[String, Int] = Left("error2")
    val success: Either[String, Int] = Right(1)
    sequenceEither(List(error1, success, error2)) shouldEqual Left("error1")
    sequenceEither(List(success, success, success)) shouldEqual Right(List(1, 1, 1))
  }
}
