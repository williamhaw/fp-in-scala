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

  test("RNG flatmap") {
    val rng = SimpleRNG(42)
    val expected = double(int(rng)._2)
    val (_, rng2) = int(rng)
    val expected2 = int(int(rng)._2)
    //value of first random number is positive
    flatMap(int)(i => if (i < 0) unit(i) else double)(rng) shouldEqual expected
    //value of first random number is negative
    flatMap(int)(i => if (i < 0) unit(i) else double)(rng2) shouldEqual expected2
  }

  test("RNG nonNegativeLessThan") {
    val rng = SimpleRNG(42)
    nonNegativeLessThan(5)(rng)._1 < 5
    nonNegativeLessThan(5)(rng)._1 > 0
  }

  test("RNG mapWithFlatMap") {
    val rng = SimpleRNG(42)
    val firstR = int(rng)
    mapWithFlatMap(int)(_ + 1)(rng)._1 shouldEqual firstR._1 + 1
  }

  test("RNG map2WithFlatMap") {
    val rng = SimpleRNG(42)
    val firstR = int(rng)
    val secondR = int(int(rng)._2)
    val (result, _) = map2WithFlatMap(int, int)(_ + _)(rng)
    result shouldEqual firstR._1 + secondR._1
  }

  sealed trait DoorState

  case object Open extends DoorState

  case object Closed extends DoorState

  case class Door(state: DoorState)

  test("State unit") {
    State.unit[Door, DoorState](Open).run(Door(Closed)) shouldEqual(Open, Door(Closed))
  }

  test("State map") {
    //if door is open, close it and return closed door
    //else open the door and return open door
    val initialState = State[Door, DoorState](d => if (d.state == Open) (Closed, Door(Closed)) else (Open, Door(Open)))
    //door open initially, after transition will be closed and mapped to 0
    initialState.map(ds => if (ds == Open) 1 else 0).run(Door(Open)) shouldEqual(0, Door(Closed))
    //door closed initially, after transition will be open and mapped to 1
    initialState.map(ds => if (ds == Open) 1 else 0).run(Door(Closed)) shouldEqual(1, Door(Open))
  }

  test("State map2") {
    //if door is open, close it and return closed door
    //else open the door and return open door
    val initialState = State[Door, DoorState](d => if (d.state == Open) (Closed, Door(Closed)) else (Open, Door(Open)))
    //leave door as is
    val initialState2 = State[Door, DoorState](d => if (d.state == Open) (Open, Door(Open)) else (Closed, Door(Closed)))

    def mapLogic(a: DoorState, b: DoorState): Int = (a, b) match {
      case (Closed, Closed) => 1
      case (Closed, Open) => 2
      case (Open, Closed) => 3
      case (Open, Open) => 4
    }

    initialState.map2(initialState2)(mapLogic).run(Door(Open)) shouldEqual(1, Door(Closed))
    initialState2.map2(initialState)(mapLogic).run(Door(Closed)) shouldEqual(2, Door(Open))
    initialState2.map2(initialState)(mapLogic).run(Door(Open)) shouldEqual(3, Door(Closed))
    initialState.map2(initialState2)(mapLogic).run(Door(Closed)) shouldEqual(4, Door(Open))
  }

  sealed trait LightState

  case object On extends LightState

  case object Off extends LightState

  test("State flatMap") {
    //if door is open, close it and return closed door
    //else open the door and return open door
    val initialState = State[Door, DoorState](d => if (d.state == Open) (Closed, Door(Closed)) else (Open, Door(Open)))

    def doorToLight(doorState: DoorState) = State[Door, LightState](d => if (doorState == Open) (On, d) else (Off, d))

    initialState.flatMap(doorToLight).run(Door(Open)) shouldEqual(Off, Door(Closed))
    initialState.flatMap(doorToLight).run(Door(Closed)) shouldEqual(On, Door(Open))
  }

  test("State sequence") {
    //if door is open, close it and return closed door
    //else open the door and return open door
    val initialState = State[Door, DoorState](d => if (d.state == Open) (Closed, Door(Closed)) else (Open, Door(Open)))
    //leave door as is
    val initialState2 = State[Door, DoorState](d => if (d.state == Open) (Open, Door(Open)) else (Closed, Door(Closed)))

    State.sequence(List(initialState, initialState2)).run(Door(Open)) shouldEqual(List(Closed, Closed), Door(Closed))
    State.sequence(List(initialState, initialState2)).run(Door(Closed)) shouldEqual(List(Open, Open), Door(Open))
  }
}
