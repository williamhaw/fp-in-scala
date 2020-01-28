package chapter7

import java.util.concurrent.{ExecutorService, Executors}

import org.scalatest.{FunSuite, Matchers}

class Chapter7Spec extends FunSuite with Matchers {

  val executorService: ExecutorService = Executors.newFixedThreadPool(2)

  test("asyncF") {
    def asyncAddTwo = Par.asyncF((a: Int) => a + 2)

    Par.run(executorService)(asyncAddTwo(5)).get() shouldEqual 7
  }

}
