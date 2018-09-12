package mmenestret.maze.algebras.impl
import cats.effect.Sync
import cats.implicits._
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import mmenestret.maze.algebras.PrintAndRead

object PrintAndReadLanternaImpl {
  def create[Effect[_]: Sync]: Effect[PrintAndRead[Effect]] = {

    val S = Sync[Effect]

    S.delay(new DefaultTerminalFactory().createTerminal()).map { term ⇒
      new PrintAndRead[Effect] {
        def putChar(c: Char): Effect[Unit] = S.delay(term.putCharacter(c))
        def flush: Effect[Unit]            = S.delay(term.flush())
        def clearScreen: Effect[Unit]      = S.delay(term.clearScreen())
        def println(str: String): Effect[Unit] =
          clearScreen *> str.toList.traverse(putChar) *> putChar('\n') *> flush
        def readStr: Effect[String] = S.delay(scala.io.StdIn.readLine()).recoverWith { case _ ⇒ readStr }
        def readInt: Effect[Int]    = S.delay(scala.io.StdIn.readInt()).recoverWith { case _  ⇒ readInt }
        def readChar: Effect[Char]  = S.delay(scala.io.StdIn.readChar()).recoverWith { case _ ⇒ readChar }
        def readKeyStrokeAsChar: Effect[Char] =
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
