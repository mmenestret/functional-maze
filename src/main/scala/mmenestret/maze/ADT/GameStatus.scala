package mmenestret.maze.ADT

sealed abstract class GameStatus

case object OnGoing            extends GameStatus
sealed abstract class Finished extends GameStatus
case object Lost               extends Finished
case object Won                extends Finished
