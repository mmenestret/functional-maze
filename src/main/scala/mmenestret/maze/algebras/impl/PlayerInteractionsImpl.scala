package mmenestret.maze.algebras.impl
import cats.effect.Sync
import cats.implicits._
import mmenestret.maze.ADT.{Azerty, Down, KeyboardLayout, Left, Move, Qwerty, Right, Up}
import mmenestret.maze.algebras.PlayerInteractions

class PlayerInteractionsImpl[Effect[+ _]: Sync] extends PlayerInteractions[Effect] {

  val S: Sync[Effect] = Sync[Effect]

  override def getPlayerInput(layout: KeyboardLayout): Effect[Move] = {
    (for {
      input ← S.delay { scala.io.StdIn.readChar() }
      move ← input match {
        case k if k == layout.upKey    ⇒ Up.pure[Effect]
        case k if k == layout.downKey  ⇒ Down.pure[Effect]
        case k if k == layout.leftKey  ⇒ Left.pure[Effect]
        case k if k == layout.rightKey ⇒ Right.pure[Effect]
        case _                         ⇒ getPlayerInput(layout)
      }
    } yield move).recoverWith { case _ ⇒ getPlayerInput(layout) }
  }

  override def printStr(str: String): Effect[Unit] = S.delay(println(str))

  override def afkForMapSize(): Effect[Int] =
    (for {
      _        ← printStr("What's the map side's size you want to play on, noob ?")
      sideSize ← S.delay(scala.io.StdIn.readInt())
    } yield sideSize).recoverWith { case _ ⇒ afkForMapSize() }

  override def afkForNumberOfTrap(): Effect[Int] =
    (for {
      _             ← printStr("How many traps ?")
      numberOfTraps ← S.delay(scala.io.StdIn.readInt())
    } yield numberOfTraps).recoverWith { case _ ⇒ afkForNumberOfTrap() }

  override def askForKeyboardLayout(): Effect[KeyboardLayout] =
    (for {
      _     <- printStr("(A)zerty or (Q)werty ? (A or Q for the idiots who didn't get it...)")
      input ← S.delay(scala.io.StdIn.readChar().toLower)
      layout = input match {
        case 'a' ⇒ Azerty
        case 'q' ⇒ Qwerty
      }
    } yield layout).recoverWith { case _ => askForKeyboardLayout() }

}
