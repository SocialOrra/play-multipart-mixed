package test

import org.scalatest.WordSpec

import scala.concurrent.duration._
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, ExecutionContext, Future }

trait TestSpec extends WordSpec {
  def assert[T](f: Future[T], timeout: Duration = 100.second, assertion: T ⇒ Boolean)(implicit ec: ExecutionContext): Unit = {
    try {
      org.scalatest.Assertions.assert(assertion(Await.result(f, timeout)))
    } catch {
      case e: Exception ⇒
        println(s"${e.getMessage}:\n${e.getStackTrace.map(_.toString).mkString("\n")}")
        org.scalatest.Assertions.assert(false)
    }
  }
}
