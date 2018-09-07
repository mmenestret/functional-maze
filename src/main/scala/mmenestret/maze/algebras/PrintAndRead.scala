package mmenestret.maze.algebras

trait PrintAndRead[Effect[_]] {
  def printStr(str: String): Effect[Unit]
  def readStr: Effect[String]
  def readInt: Effect[Int]
  def readChar: Effect[Char]
}

object PrintAndRead {
  def apply[Effect[_]: PrintAndRead]: PrintAndRead[Effect] = implicitly[PrintAndRead[Effect]]
}
