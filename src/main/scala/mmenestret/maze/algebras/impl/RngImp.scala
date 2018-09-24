package mmenestret.maze.algebras.impl
import cats.effect.Sync
import mmenestret.maze.algebras.Rng

import scala.util.Random

object RngImp {
  def apply[F[_]: Sync](implicit S: Sync[F]): Rng[F] =
    (low: Int, high: Int) => S.delay(Random.nextInt(high - low) + low)
}
