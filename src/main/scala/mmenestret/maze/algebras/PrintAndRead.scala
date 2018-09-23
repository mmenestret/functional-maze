package mmenestret.maze.algebras

trait PrintAndRead[F[_]] {
  def clearAndPrintln(str: String): F[Unit]
  def clearScreen(): F[Unit]
  def println(str: String): F[Unit]
  def readStr: F[String]
  def readInt: F[Int]
  def readChar: F[Char]
  def readKeyStrokeAsChar: F[Char]
}

object PrintAndRead {
  def apply[F[_]: PrintAndRead]: PrintAndRead[F] = implicitly[PrintAndRead[F]]
}
