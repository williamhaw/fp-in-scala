import scala.math.max

package object chapter3 {

  //Exercise 3.2
  def tail[T](list: List[T]): List[T] = list match {
    case Nil => throw new IllegalArgumentException("Tail of empty list")
    case head :: Nil => Nil
    case head :: tail => tail
  }

  //Exercise 3.3
  def setHead[T](head: T, list: List[T]): List[T] = list match {
    case Nil => throw new IllegalArgumentException("setHead() called on empty list")
    case _ :: tail => head :: tail
  }

  //Exercise 3.4
  @scala.annotation.tailrec
  def drop[T](l: List[T], n: Int): List[T] = l match {
    case Nil => Nil
    case _ if (n <= 0) => l
    case _ :: tail => drop(tail, n - 1)
  }

  //Exercise 3.5
  @scala.annotation.tailrec
  def dropWhile[T](l: List[T], f: T => Boolean): List[T] = l match {
    case head :: tail if f(head) => dropWhile(tail, f)
    case _ => l
  }

  //Exercise 3.6
  def init[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case head :: tail => head :: init(tail)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  //Exercise 3.9
  def length[T](as: List[T]): Int = foldRight(as, 0)((_: T, acc: Int) => acc + 1)

  //Exercise 3.10
  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case head :: tail => foldLeft(tail, f(z, head))(f)
    case Nil => z
  }

  //Exercise 3.11
  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  //Exercise 3.12
  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => h :: acc)

  //Exercise 3.13
  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(a, b)))(z)

  //Exercise 3.14
  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(_ :: _)

  //Exercise 3.15
  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  //Exercise 3.16
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => h + 1 :: t)

  //Exercise 3.17
  def doubleListToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => h.toString :: t)

  //Exercise 3.18
  def map[A, B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => f(h) :: t)

  //Exercise 3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if (f(h)) h :: t else t)

  //Exercise 3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  //Exercise 3.21
  def filterUsingFlatmap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)(x => if (f(x)) List(x) else List())

  //Exercise 3.22
  def addCorresponding(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => h1 + h2 :: addCorresponding(t1, t2)
  }

  //Exercise 3.23
  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (h1 :: t1, h2 :: t2) => f(h1, h2) :: zipWith(t1, t2)(f)
  }

  //Exercise 3.24
  @scala.annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (Nil, Nil) => true
    case (Nil, _) => false
    case (_, Nil) => true
    case (h1 :: t1, h2 :: t2) => if (h1 == h2) hasSubsequence(t1, t2) else hasSubsequence(t1, sub)
  }

  sealed trait Tree[+A]

  case class Leaf[A](value: A) extends Tree[A]

  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  //Exercise 3.25
  def sizeOfTree[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => sizeOfTree(l) + sizeOfTree(r) + 1
  }

  //Exercise 3.26
  def maxOfTree(tree: Tree[Int]): Int = tree match {
    case Leaf(v) => v
    case Branch(l, r) => max(maxOfTree(l), maxOfTree(r))
  }

  //Exercise 3.27
  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => max(depth(l) + 1, depth(r) + 1)
  }

  //Exercise 3.28
  def treeMap[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(v) => Leaf(f(v))
    case Branch(l, r) => Branch(treeMap(l)(f), treeMap(r)(f))
  }

  //Exercise 3.29
  def treeFold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(v) => f(v)
    case Branch(l, r) => g(treeFold(l)(f)(g), treeFold(r)(f)(g))
  }

  def treeSizeUsingFold[A](tree: Tree[A]): Int =
    treeFold(tree)(_ => 1)((sizeLeft, sizeRight) => sizeLeft + sizeRight + 1)

  def treeMaxUsingFold[A](tree: Tree[Int]): Int =
    treeFold(tree)(e => e)((maxLeft, maxRight) => max(maxLeft, maxRight))

  def treeDepthUsingFold[A](tree: Tree[A]): Int =
    treeFold(tree)(_ => 1)((depthLeft, depthRight) => 1 + max(depthLeft, depthRight))

  def treeMapUsingFold[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    treeFold(tree)(v => Leaf(f(v)): Tree[B])((mapLeft, mapRight) => Branch(mapLeft, mapRight))
}
