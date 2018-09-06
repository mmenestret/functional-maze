package mmenestret.maze.algebras.impl
import cats.effect.IO
import cats.implicits._
import mmenestret.maze.ADT.{Azerty, Down, KeyboardLayout, Left, Move, Qwerty, Right, Up}
import mmenestret.maze.algebras.PlayerInteractions

trait PlayerInteractionsImpl extends PlayerInteractions[IO] {
  override def getPlayerInput(layout: KeyboardLayout): IO[Move] = {
    IO { scala.io.StdIn.readChar() }
      .flatMap {
        case k if k == layout.upKey    ⇒ Up.pure[IO]
        case k if k == layout.downKey  ⇒ Down.pure[IO]
        case k if k == layout.leftKey  ⇒ Left.pure[IO]
        case k if k == layout.rightKey ⇒ Right.pure[IO]
        case _                         ⇒ getPlayerInput(layout)
      }
      .recoverWith { case _ ⇒ getPlayerInput(layout) }
  }

  override def printStr(str: String): IO[Unit] = IO(println(str))

  override def afkForMapSize(): IO[Int] =
    (for {
      _        ← printStr("What's the map side's size you want to play on, noob ?")
      sideSize ← IO(scala.io.StdIn.readInt())
    } yield sideSize).recoverWith { case _ ⇒ afkForMapSize() }

  override def afkForNumberOfTrap(): IO[Int] =
    (for {
      _             ← printStr("How many traps ?")
      numberOfTraps ← IO(scala.io.StdIn.readInt())
    } yield numberOfTraps).recoverWith { case _ ⇒ afkForNumberOfTrap() }

  override def askForKeyboardLayout(): IO[KeyboardLayout] =
    (for {
      _     <- printStr("(A)zerty or (Q)werty ? (A or Q for the idiots who didn't get it...)")
      input ← IO(scala.io.StdIn.readChar().toLower)
      layout = input match {
        case 'a' ⇒ Azerty
        case 'q' ⇒ Qwerty
      }
    } yield layout).recoverWith { case _ => askForKeyboardLayout() }

}
