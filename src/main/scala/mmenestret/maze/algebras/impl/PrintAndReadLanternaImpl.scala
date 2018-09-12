package mmenestret.maze.algebras.impl
import cats.effect.Sync
import cats.implicits._
import com.googlecode.lanterna.input.{KeyStroke, KeyType}
import com.googlecode.lanterna.terminal.DefaultTerminalFactory
import mmenestret.maze.algebras.PrintAndRead

object PrintAndReadLanternaImpl {
  def create[Effect[_]: Sync]: Effect[PrintAndRead[Effect]] = {

    val S = Sync[Effect]

    S.delay(new DefaultTerminalFactory().createTerminal()).map { term ⇒
      new PrintAndRead[Effect] {
        def putChar(c: Char): Effect[Unit]   = S.delay(term.putCharacter(c))
        def flush: Effect[Unit]              = S.delay(term.flush())
        def readKeyStroke: Effect[KeyStroke] = S.delay(term.readInput())
        override def println(str: String): Effect[Unit] =
          str.toList.traverse(putChar) *> putChar('\n') *> flush
        override def readStr: Effect[String] = {
          def loop(acc: String): Effect[String] = {
            for {
              k ← readKeyStroke
              input ← if (k.getKeyType == KeyType.Enter) S.pure(acc)
              else {
                Option(k.getCharacter).map(_.toChar) match {
                  case Some(value) ⇒ putChar(value) *> loop(value + acc)
                  case None        ⇒ S.raiseError(new Throwable("Not a char"))
                }
              }
            } yield input
          }
          loop("")
        }
        override def readInt: Effect[Int] = ???
        override def readChar: Effect[Char] = {
          for {
            kopt ← readKeyStroke.map(k ⇒ Option(k.getCharacter.toChar))
            c ← kopt match {
              case Some(value) ⇒ S.pure(value)
              case None        ⇒ S.raiseError(new Throwable("Not a char"))
            }
            _ ← putChar(c)
          } yield c
        }
      }
    }
  }
}
