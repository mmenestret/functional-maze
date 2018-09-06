package mmenestret.maze.algebras
import cats.Applicative
import cats.implicits._

trait Rng[A[_]] {
  def generateRngBetween(low: Int, high: Int): A[Int]
  def generateNRngBetween(n: Int)(low: Int, high: Int)(implicit app: Applicative[A]): A[List[Int]] =
    (0 until n).toList.traverse(_ ⇒ generateRngBetween(low, high))
}
object Rng {
  def apply[A[_]: Rng]: Rng[A] = implicitly[Rng[A]]
}
