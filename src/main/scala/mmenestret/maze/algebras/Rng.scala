package mmenestret.maze.algebras
import cats.Applicative
import cats.implicits._

trait Rng[F[_]] {
  def generateRngBetween(low: Int, high: Int): F[Int]
  def generateNRngBetween(n: Int)(low: Int, high: Int)(implicit A: Applicative[F]): F[List[Int]] =
    (0 until n).toList.traverse(_ ⇒ generateRngBetween(low, high))
}
object Rng {
  def apply[F[_]: Rng]: Rng[F] = implicitly[Rng[F]]
}
