package mmenestret.maze.algebras.impl
import cats.effect.IO
import mmenestret.maze.algebras.Rng

import scala.util.Random

trait RngImp extends Rng[IO] {
  override def generateRngBetween(low: Int, high: Int): IO[Int] = IO(Random.nextInt(high - low) + low)
}
