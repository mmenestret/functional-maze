package mmenestret.maze.ADT

sealed abstract class Move
object Move {
  val up: Move    = Up
  val down: Move  = Down
  val left: Move  = Left
  val right: Move = Right
}
case object Up    extends Move
case object Down  extends Move
case object Left  extends Move
case object Right extends Move
