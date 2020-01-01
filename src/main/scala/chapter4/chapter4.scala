package object chapter4 {

  sealed trait Option[+A] {
    //Exercise 4.1
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

  //Exercise 4.2
  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(l: Seq[Double]): Option[Double] = if (l.isEmpty) None else Some(l.sum / l.length)

    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  //Exercise 4.3
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aRaw => b.map(bRaw => f(aRaw, bRaw)))

  //Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => h.flatMap(hRaw => sequence(t).map(rawList => hRaw :: rawList))
  }

  //Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequenceUsingTraverse[A](a: List[Option[A]]): Option[List[A]] = traverse(a)(x => x)

  sealed trait Either[+E, +A] {
    //Exercise 4.6
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

  //Exercise 4.7
  def sequenceEither[E, A](es: List[Either[E, A]]): Either[E, List[A]] = traverseEither(es)(x => x)

  def traverseEither[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverseEither(t)(f))(_ :: _)
  }
}
