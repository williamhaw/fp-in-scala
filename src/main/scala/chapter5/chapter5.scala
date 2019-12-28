import chapter5.Stream._

package object chapter5 {

  sealed trait Stream[+A] {
    def toList: List[A] = this match {
      case Cons(h, t) => h() :: t().toList
      case Empty => List()
    }

    def take(n: Int): Stream[A] = this match {
      case Cons(h, t) if n > 0 => cons[A](h(), t().take(n - 1))
      case Cons(_, _) if n <= 0 => empty
      case _ => empty
    }

    def drop(n: Int): Stream[A] = this match {
      case Cons(_, t) if n > 0 => t().drop(n - 1)
      case Cons(_, _) if n <= 0 => this
      case _ => empty
    }

    def takeWhile(p: A => Boolean): Stream[A] = this match {
      case Cons(h, t) if(p(h())) => cons[A](h(), t().takeWhile(p))
      case Cons(_, t) => t().takeWhile(p)
      case _ => empty
    }
  }

  case object Empty extends Stream[Nothing]

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

}
