package mmenestret.maze.algebras.impl
import cats.effect.Sync
import cats.implicits._
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import mmenestret.maze.algebras.PrintAndRead

object PrintAndReadLanternaImpl {

  def initiate[F[_]](implicit S: Sync[F]): F[PrintAndRead[F]] = {

    S.delay(new DefaultTerminalFactory().createTerminal()).map { term ⇒
      new PrintAndRead[F] {
        // Utility
        def putChar(c: Char): F[Unit] = S.delay(term.putCharacter(c))
        def flush: F[Unit]            = S.delay(term.flush())
        // PrintAndRead
        def clearScreen(): F[Unit]        = S.delay(term.clearScreen())
        def println(str: String): F[Unit] = str.toList.traverse(putChar) *> putChar('\n') *> flush
        def readStr: F[String]            = S.delay(scala.io.StdIn.readLine()).recoverWith { case _ ⇒ readStr }
        def readInt: F[Int]               = S.delay(scala.io.StdIn.readInt()).recoverWith { case _ ⇒ readInt }
        def readChar: F[Char]             = S.delay(scala.io.StdIn.readChar()).recoverWith { case _ ⇒ readChar }
        def readKeyStrokeAsChar: F[Char] =
          for {
            ksOpt ← S.delay(Option(term.pollInput().getCharacter).map(_.toChar))
            ks ← ksOpt match {
              case Some(k) ⇒ S.pure(k)
              case _       ⇒ readKeyStrokeAsChar
            }
          } yield ks
      }
    }
  }

}
