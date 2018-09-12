package mmenestret.maze.algebras.impl
import cats.effect.Sync
import mmenestret.maze.algebras.Rng

import scala.util.Random

object RngImp {

  def apply[Effect[_]: Sync]: Rng[Effect] = new Rng[Effect] {

    val S: Sync[Effect] = Sync[Effect]

    override def generateRngBetween(low: Int, high: Int): Effect[Int] = S.delay(Random.nextInt(high - low) + low)
  }

}
