package mmenestret.maze.algebras.impl
import cats.effect.Sync
import mmenestret.maze.algebras.PrintAndRead

class PrintAndReadImpl[Effect[_]: Sync] extends PrintAndRead[Effect] {

  val S: Sync[Effect] = Sync[Effect]

  def printStr(str: String): Effect[Unit] = S.delay(println(str))
  def readStr: Effect[String]             = S.delay(scala.io.StdIn.readLine())
  def readInt: Effect[Int]                = S.delay(scala.io.StdIn.readInt())
  def readChar: Effect[Char]              = S.delay(scala.io.StdIn.readChar())
}
