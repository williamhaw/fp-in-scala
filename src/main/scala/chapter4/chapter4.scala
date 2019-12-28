package chapter4

package object chapter4 {

  sealed trait Option[+A]{
    def map[B](f: A => B): Option[B] = this match {
      case None => None
      case Some(v) => Some(f(v))
    }
    def flatMap[B](f: A => Option[B]): Option[B] = map(f) getOrElse None
    def getOrElse[B >: A](default: => B): B = this match {
      case None => default
      case Some(v) => v
    }
    def orElse[B >: A](ob: => Option[B]): Option[B] = this map(Some(_)) getOrElse(ob)
    def filter(f: A => Boolean): Option[A] = this match {
      case Some(v) if f(v) => this
      case _ => None
    }
  }
  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  def variance(xs: Seq[Double]): Option[Double] = {
    def mean(l: Seq[Double]): Option[Double] = if(l.isEmpty) None else Some(l.sum / l.length)
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(aRaw => b.map(bRaw => f(aRaw, bRaw)))
}
