package mmenestret.maze.algebras.impl
import cats.MonadError
import cats.implicits._
import mmenestret.maze.ADT.{Azerty, Down, KeyboardLayout, Left, Move, Qwerty, Right, Up}
import mmenestret.maze.algebras.{PlayerInteractions, PrintAndRead}

object PlayerInteractionsImpl {

  def apply[Effect[+ _]: PrintAndRead: MonadError[?[_], Throwable]]: PlayerInteractions[Effect] =
    new PlayerInteractions[Effect] {

      val PR: PrintAndRead[Effect] = PrintAndRead[Effect]

      override def displayEndMessage(msg: String): Effect[Unit] = PR.println(msg)

      override def displayMap(mapAsString: String): Effect[Unit] = PR.clearAndPrintln(mapAsString)

      override def askPlayerDirection(layout: KeyboardLayout): Effect[Move] = {
        (for {
          input ← PR.readKeyStrokeAsChar
          move ← input match {
            case k if k == layout.upKey    ⇒ Up.pure[Effect]: Effect[Move]
            case k if k == layout.downKey  ⇒ Down.pure[Effect]
            case k if k == layout.leftKey  ⇒ Left.pure[Effect]
            case k if k == layout.rightKey ⇒ Right.pure[Effect]
            case _                         ⇒ askPlayerDirection(layout)
          }
        } yield move).recoverWith { case _ ⇒ askPlayerDirection(layout) }
      }

      override def afkForMapSize(): Effect[Int] =
        PR.println("What's the map side's size you want to play on, noob ?") *> PR.readInt

      override def afkForNumberOfTrap(): Effect[Int] = PR.println("How many traps ?") *> PR.readInt

      override def askForKeyboardLayout(): Effect[KeyboardLayout] =
        (PR.println("(A)zerty or (Q)werty ? (A or Q for the idiots who didn't get it...)") *> PR.readChar.map(
          _.toLower))
          .flatMap {
            case 'a' ⇒ Azerty.pure[Effect]
            case 'q' ⇒ Qwerty.pure[Effect]
            case _   ⇒ askForKeyboardLayout()
          }
    }

}
