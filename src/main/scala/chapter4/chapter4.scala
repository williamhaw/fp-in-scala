package object chapter4 {

  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }

    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None

    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = this map (Some(_)) getOrElse (ob)

    def filter(f: A => Boolean): Option[A] = this match {
      case Some(v) if f(v) => this
      case _ => None
    }
  }

  case class Some[+A](get: A) extends Option[A]

  case object None extends Option[Nothing]

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(l: Seq[Double]): Option[Double] = if (l.isEmpty) None else Some(l.sum / l.length)

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aRaw => b.map(bRaw => f(aRaw, bRaw)))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hRaw => sequence(t).map(rawList => hRaw :: rawList))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => Right(f(v))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
      case Left(e) => Left(e)
      case Right(v) => f(v)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
      case Left(_) => b
      case Right(v) => Right(v)
    }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] = for {
      a <- this
      bRaw <- b
    } yield {
      f(a, bRaw)
    }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]

  case class Right[+A](value: A) extends Either[Nothing, A]

}
