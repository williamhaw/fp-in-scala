package chapter2

import org.scalatest.{FlatSpec, Matchers}

class Chapter2Spec extends FlatSpec with Matchers{
 "fib" should "get the right value for the nth Fibonacci number" in {
   fib(5) shouldEqual 5
 }

  "isSorted" should "return true if array is sorted" in {
    isSorted(Array(1, 2, 3), (a : Int, b : Int) => a < b) shouldEqual true
    isSorted(Array(3, 2, 1), (a : Int, b : Int) => a > b) shouldEqual true
  }

  "isSorted" should "return false if array is not sorted" in {
    isSorted(Array(1, 2, 3), (a : Int, b : Int) => a > b) shouldEqual false
    isSorted(Array(1, 3, 2), (a : Int, b : Int) => a < b) shouldEqual false
  }

}
