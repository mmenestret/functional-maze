package mmenestret.maze.algebras
import mmenestret.maze.ADT.{KeyboardLayout, Move}

trait PlayerInteractions[F[_]] {
  def displayMap(mapAsString: String): F[Unit]
  def displayEndMessage(msg: String): F[Unit]
  def askPlayerDirection(layout: KeyboardLayout): F[Move]
  def afkForMapSize(): F[Int]
  def afkForNumberOfTrap(): F[Int]
  def askForKeyboardLayout(): F[KeyboardLayout]
  def clearPlayerScreen(): F[Unit]
}
object PlayerInteractions {
  def apply[F[_]: PlayerInteractions]: PlayerInteractions[F] = implicitly[PlayerInteractions[F]]
}
