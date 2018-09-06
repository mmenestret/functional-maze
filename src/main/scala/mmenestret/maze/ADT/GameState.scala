package mmenestret.maze.ADT

sealed trait GameState
case object Ongoing   extends GameState
sealed trait Finished extends GameState
case object Lost      extends Finished
case object Won       extends Finished
