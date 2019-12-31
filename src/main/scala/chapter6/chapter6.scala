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
}
