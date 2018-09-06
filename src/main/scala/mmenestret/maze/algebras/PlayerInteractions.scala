package mmenestret.maze.algebras
import mmenestret.maze.ADT.{KeyboardLayout, Move}

trait PlayerInteractions[A[_]] {
  def printStr(str: String): A[Unit]
  def getPlayerInput(layout: KeyboardLayout): A[Move]
  def afkForMapSize(): A[Int]
  def afkForNumberOfTrap(): A[Int]
  def askForKeyboardLayout(): A[KeyboardLayout]
}
object PlayerInteractions {
  def apply[A[_]: PlayerInteractions]: PlayerInteractions[A] = implicitly[PlayerInteractions[A]]
}
