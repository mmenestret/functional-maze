package mmenestret.maze.algebras.impl
import cats.MonadError
import cats.implicits._
import mmenestret.maze.ADT.{Azerty, Down, KeyboardLayout, Left, Move, Qwerty, Right, Up}
import mmenestret.maze.algebras.{PlayerInteractions, PrintAndRead}

class PlayerInteractionsImpl[Effect[+ _]: PrintAndRead: MonadError[?[_], Throwable]]
    extends PlayerInteractions[Effect] {

  val PR: PrintAndRead[Effect] = PrintAndRead[Effect]

  override def displayEndMessage(msg: String): Effect[Unit] = PR.printStr(msg)

  override def displayMap(mapAsString: String): Effect[Unit] = PR.printStr(mapAsString)

  override def askPlayerDirection(layout: KeyboardLayout): Effect[Move] = {
    (for {
      input ← PR.readChar
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
    (for {
      _        ← PR.printStr("What's the map side's size you want to play on, noob ?")
      sideSize ← PR.readInt
    } yield sideSize).recoverWith { case _ ⇒ afkForMapSize() }

  override def afkForNumberOfTrap(): Effect[Int] =
    (for {
      _             ← PR.printStr("How many traps ?")
      numberOfTraps ← PR.readInt
    } yield numberOfTraps).recoverWith { case _ ⇒ afkForNumberOfTrap() }

  override def askForKeyboardLayout(): Effect[KeyboardLayout] =
    (for {
      _     <- PR.printStr("(A)zerty or (Q)werty ? (A or Q for the idiots who didn't get it...)")
      input ← PR.readChar.map(_.toLower)
      layout = input match {
        case 'a' ⇒ Azerty
        case 'q' ⇒ Qwerty
      }
    } yield layout).recoverWith { case _ => askForKeyboardLayout() }

}
