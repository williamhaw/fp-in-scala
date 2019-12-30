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
      case Cons(h, t) if (p(h())) => cons[A](h(), t().takeWhile(p))
      case Cons(_, t) => t().takeWhile(p)
      case _ => empty
    }

    def forall(p: A => Boolean): Boolean = this match {
      case Cons(h, t) if p(h()) => t().forall(p)
      case Cons(_, _) => false
      case _ => true
    }

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def takeWhileWithFoldRight(p: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (p(h)) cons[A](h, t) else t)

    def headOptionWithFoldRight: Option[A] =
      foldRight[Option[A]](None)((h, _) => Some(h))

    def map[B](f: A => B): Stream[B] =
      foldRight(empty[B])((h, t) => cons(f(h), t))

    def filter(f: A => Boolean): Stream[A] =
      foldRight(empty[A])((h, t) => if (f(h)) cons(h, t) else t)

    def append[B >: A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(empty[B])((h, t) => f(h).append(t))

    def mapUsingUnfold[B](f: A => B): Stream[B] = unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

    def takeUsingUnfold(n: Int): Stream[A] = unfold((this, n)) {
      case (Cons(h, _), 1) => Some(h(), (empty, 0))
      case (Cons(h, t), n) if n > 1 => Some(h(), (t(), n - 1))
      case _ => None
    }

    def takeWhileUsingUnfold(p: A => Boolean): Stream[A] = unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case Cons(_, t) => t().headOptionWithFoldRight.filter(p).map(at => (at, t().drop(1).takeWhileUsingUnfold(p)))
      case _ => None
    }

    def zipWith[B, C](b: Stream[B])(f: (A, B) => C): Stream[C] = unfold(this, b) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

    def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] = {
      def all[C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] = unfold(this, s2) {
        case (Empty, Empty) => None
        case (Cons(h, t), Empty) => Some((f(Some(h()), None), (t(), Empty)))
        case (Empty, Cons(h, t)) => Some((f(None, Some(h())), (Empty, t())))
        case (Cons(h1, t1), Cons(h2, t2)) => Some((f(Some(h1()), Some(h2())), (t1(), t2())))
      }

      all(s2)((_, _))
    }

    def startsWith[A](s: Stream[A]): Boolean = zipAll(s).takeWhile(_._2.isDefined).forall { case (h1, h2) => h1 == h2 }
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

  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  def fibs: Stream[Int] = {
    def loop(a: Int, b: Int): Stream[Int] = cons(a, loop(b, a + b))

    loop(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case Some((h, s)) => cons(h, unfold(s)(f))
    case None => empty
  }

  def fibsUsingUnfold: Stream[Int] = unfold((0, 1)) { case (a, b) => Some((a, (b, a + b))) }

  def fromUsingUnfold(n: Int): Stream[Int] = unfold(n)(a => Some(a, a + 1))

  def constantUsingUnfold[A](a: A): Stream[A] = unfold(a)(aa => Some(aa, aa))

  def onesUsingUnfold: Stream[Int] = unfold(1)(_ => Some(1, 1))
}
