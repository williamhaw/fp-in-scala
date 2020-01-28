import java.util.concurrent.{Callable, ExecutorService, Future}

import scala.concurrent.duration.TimeUnit

package object chapter7 {

  object Par {

    type Par[A] = ExecutorService => Future[A]

    //Exercise 7.1
    def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

    def unit[A](a: A): Par[A] = (es: ExecutorService) => UnitFuture(a)

    def fork[A](a: Par[A]): Par[A] = es => es.submit(new Callable[A] {
      def call: A = a(es).get
    })

    def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

    def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

    private case class UnitFuture[A](get: A) extends Future[A] {
      def isDone = true

      def get(timeout: Long, units: TimeUnit): A = get

      def isCancelled = false

      def cancel(evenIfRunning: Boolean): Boolean = false
    }

    //Exercise 7.4
    def asyncF[A, B](f: A => B): A => Par[B] = a => lazyUnit(f(a))

  }

}
