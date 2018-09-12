package mmenestret.maze.algebras

trait PrintAndRead[Effect[_]] {
  def clearAndPrintln(str: String): Effect[Unit]
  def println(str: String): Effect[Unit]
  def readStr: Effect[String]
  def readInt: Effect[Int]
  def readChar: Effect[Char]
  def readKeyStrokeAsChar: Effect[Char]
}

object PrintAndRead {
  def apply[Effect[_]: PrintAndRead]: PrintAndRead[Effect] = implicitly[PrintAndRead[Effect]]
}
