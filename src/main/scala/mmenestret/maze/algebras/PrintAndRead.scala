package mmenestret.maze.algebras
import cats.Applicative
import cats.implicits._

abstract class PrintAndRead[F[_]: Applicative]() {
  def clearScreen(): F[Unit]
  def println(str: String): F[Unit]
  def readStr: F[String]
  def readInt: F[Int]
  def readChar: F[Char]
  def readKeyStrokeAsChar: F[Char]
  def clearAndPrintln(str: String): F[Unit] = clearScreen *> println(str)
}

object PrintAndRead {
  def apply[F[_]: PrintAndRead]: PrintAndRead[F] = implicitly[PrintAndRead[F]]
}
