package mmenestret.maze.algebras.impl
import cats.effect.Sync
import mmenestret.maze.algebras.Rng

import scala.util.Random

object RngImp {

  def apply[F[_]: Sync]: Rng[F] = new Rng[F] {

    val S: Sync[F] = Sync[F]

    override def generateRngBetween(low: Int, high: Int): F[Int] = S.delay(Random.nextInt(high - low) + low)
  }

}
