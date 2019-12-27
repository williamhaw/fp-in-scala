package object chapter3 {

  def tail[T](list: List[T]): List[T] = list match {
    case Nil => throw new IllegalArgumentException("Tail of empty list")
    case head :: Nil => Nil
    case head :: tail => tail
  }

  def setHead[T](head: T, list: List[T]): List[T] = list match {
    case Nil => throw new IllegalArgumentException("setHead() called on empty list")
    case _ :: tail => head :: tail
  }

  @scala.annotation.tailrec
  def drop[T](l: List[T], n: Int): List[T] = l match {
    case Nil => Nil
    case _ if(n <= 0) => l
    case _ :: tail => drop(tail, n-1)
  }

  @scala.annotation.tailrec
  def dropWhile[T](l: List[T], f: T => Boolean): List[T] = l match {
      case head :: tail if f(head) => dropWhile(tail, f)
      case _ => l
  }

  def init[T](l: List[T]): List[T] = l match {
    case Nil => Nil
    case _ :: Nil => Nil
    case head :: tail => head :: init(tail)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }

  def length[T](as: List[T]): Int = foldRight(as, 0)((_: T, acc: Int) => acc + 1)

  @scala.annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case head :: tail => foldLeft(tail, f(z, head))(f)
    case Nil => z
  }

  def sum3(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def length2[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => h :: acc)

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b, a) => f(a, b))

  def foldLeftUsingFoldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldRight(as, (b: B) => b)((a, g) => b => g(f(a, b)))(z)

  def append[A](l: List[A], r: List[A]): List[A] = foldRight(l, r)(_ :: _)

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, List[A]())(append)

  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((h, t) => h + 1 :: t)

  def doubleListToString(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((h, t) => h.toString :: t)

  def map[A,B](as: List[A])(f: A => B): List[B] = foldRight(as, Nil: List[B])((h, t) => f(h) :: t)

  def filter[A](as: List[A])(f: A => Boolean): List[A] = foldRight(as, Nil: List[A])((h, t) => if(f(h)) h :: t else t)

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = concat(map(as)(f))

  def filterUsingFlatmap[A](as: List[A])(f: A => Boolean): List[A] = flatMap(as)( x => if(f(x)) List(x) else List())
}
