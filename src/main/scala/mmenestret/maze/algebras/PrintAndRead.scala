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
  def apply[F[_]: PrintAndRead]: PrintAndRead[F]                               = implicitly[PrintAndRead[F]]
  def clearAndPrintln[F[_]](str: String)(implicit F: PrintAndRead[F]): F[Unit] = F.clearAndPrintln(str)
  def clearScreen[F[_]]()(implicit F: PrintAndRead[F]): F[Unit]                = F.clearScreen()
  def println[F[_]](str: String)(implicit F: PrintAndRead[F]): F[Unit]         = F.println(str)
  def readStr[F[_]](implicit F: PrintAndRead[F]): F[String]                    = F.readStr
  def readInt[F[_]](implicit F: PrintAndRead[F]): F[Int]                       = F.readInt
  def readChar[F[_]](implicit F: PrintAndRead[F]): F[Char]                     = F.readChar
  def readKeyStrokeAsChar[F[_]](implicit F: PrintAndRead[F]): F[Char]          = F.readKeyStrokeAsChar
}
