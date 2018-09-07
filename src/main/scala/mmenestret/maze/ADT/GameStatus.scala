package mmenestret.maze.ADT

sealed trait GameStatus
case object OnGoing   extends GameStatus
sealed trait Finished extends GameStatus
case object Lost      extends Finished
case object Won       extends Finished
