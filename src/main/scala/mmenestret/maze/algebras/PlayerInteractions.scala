package mmenestret.maze.algebras
import mmenestret.maze.ADT.{KeyboardLayout, Move}

trait PlayerInteractions[Effect[_]] {
  def displayMap(mapAsString: String): Effect[Unit]
  def displayEndMessage(msg: String): Effect[Unit]
  def askPlayerDirection(layout: KeyboardLayout): Effect[Move]
  def afkForMapSize(): Effect[Int]
  def afkForNumberOfTrap(): Effect[Int]
  def askForKeyboardLayout(): Effect[KeyboardLayout]
  def clearPlayerScreen(): Effect[Unit]
}
object PlayerInteractions {
  def apply[Effect[_]: PlayerInteractions]: PlayerInteractions[Effect] = implicitly[PlayerInteractions[Effect]]
}
