package object chapter6 {

  trait RNG {
    def nextInt: (Int, RNG)
  }

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  //Exercise 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    if (nextInt >= 0)
      (nextInt, nextRng)
    else
      (-(nextInt + 1), nextRng)
  }

  //Exercise 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (nextInt, nextRng) = nonNegativeInt(rng)
    (nextInt / (Int.MaxValue.toDouble + 1), nextRng)
  }

  //Exercise 6.3
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (nextInt, nextRng) = rng.nextInt
    val (nextDouble, finalRng) = double(nextRng)
    ((nextInt, nextDouble), finalRng)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (nextDouble, nextRng) = double(rng)
    val (nextInt, finalRng) = nextRng.nextInt
    ((nextDouble, nextInt), finalRng)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (nextDouble1, nextRng) = double(rng)
    val (nextDouble2, nextNextRng) = double(nextRng)
    val (nextDouble3, finalRng) = double(nextNextRng)
    ((nextDouble1, nextDouble2, nextDouble3), finalRng)
  }

  //Exercise 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    if (count == 0) {
      (List.empty, rng)
    } else {
      val (nextInt, nextRng) = rng.nextInt
      val (tail, nextNextRng) = ints(count - 1)(nextRng)
      (nextInt :: tail, nextNextRng)
    }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] = rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  //Exercise 6.5
  def doubleUsingMap: Rand[Double] = map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))

  //Exercise 6.6
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rngA) = ra(rng)
      val (b, rngB) = rb(rngA)
      (f(a, b), rngB)
    }

  //Exercise 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((head, tail) => map2(head, tail) { case (h, t) => h :: t })

  //Exercise 6.8
  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
    rng => {
      val (a, nextRng) = f(rng)
      g(a)(nextRng)
    }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
    }

  //Exercise 6.9
  def mapWithFlatMap[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s) { a => unit(f(a)) }

  def map2WithFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra) { a => map(rb)(b => f(a, b)) }


  //Exercise 6.10
  case class State[S, +A](run: S => (A, S)) {

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(
      s => {
        val (a, nextS) = run(s)
        f(a).run(nextS)
      }
    )

    def map[B](f: A => B): State[S, B] =
      flatMap(a => State.unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))
  }

  object State {
    def unit[S, A](a: A): State[S, A] = State(s => (a, s))

    def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
      fs.foldRight(unit[S, List[A]](List[A]()))((head, tail) => head.map2(tail) { case (h, t) => h :: t })
  }

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  sealed trait Input

  case object Coin extends Input

  case object Turn extends Input

  case class Machine(locked: Boolean, candies: Int, coins: Int)

  //Exercise 6.11
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    for {
      _ <- State.sequence(inputs.map(input => modify[Machine](m => updateMachine(input, m))))
      s <- get
    } yield (s.coins, s.candies)

  def updateMachine(input: Input, machine: Machine): Machine = (input, machine) match {
    //A machine that is out of candy ignores all inputs.
    case (_, Machine(_, 0, _)) => machine
    //Inserting a coin into a locked machine will cause it to unlock if there is any candy left.
    case (Coin, Machine(true, candies, coins)) => Machine(locked = false, candies, coins + 1)
    //Turning the knob on an unlocked machine will cause it to dispense candy and become locked.
    case (Turn, Machine(false, candies, coins)) => Machine(locked = true, candies - 1, coins)
    //Turning the knob on a locked machine or inserting a coin into an unlocked machine does nothing.
    case (Turn, Machine(true, _, _)) => machine
    case (Coin, Machine(false, _, _)) => machine
  }

}
