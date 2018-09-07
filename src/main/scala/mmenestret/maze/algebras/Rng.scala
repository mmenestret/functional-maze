package mmenestret.maze.algebras
import cats.Applicative
import cats.implicits._

trait Rng[Effect[_]] {
  def generateRngBetween(low: Int, high: Int): Effect[Int]
  def generateNRngBetween(n: Int)(low: Int, high: Int)(implicit app: Applicative[Effect]): Effect[List[Int]] =
    (0 until n).toList.traverse(_ ⇒ generateRngBetween(low, high))
}
object Rng {
  def apply[Effect[_]: Rng]: Rng[Effect] = implicitly[Rng[Effect]]
}
