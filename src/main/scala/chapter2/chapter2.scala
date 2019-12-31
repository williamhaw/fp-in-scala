import scala.annotation.tailrec

package object chapter2 {
  //Exercise 2.1
  //get nth Fibonacci number with tail recursion
  def fib(n: Int): Int = {
    @tailrec
    def loop(n: Int, prev: Int, curr: Int): Int = {
      if (n <= 0) prev
      else loop(n - 1, curr, curr + prev)
    }

    loop(n, 0, 1)
  }

  //Exercise 2.2
  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    @scala.annotation.tailrec
    def loop(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else loop(n + 1)
    }

    loop(0)
  }

  //Exercise 2.3
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = a => f(a, _)

  //Exercise 2.4
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  //Exercise 2.5
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}