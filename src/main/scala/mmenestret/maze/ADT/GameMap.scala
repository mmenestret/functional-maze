package mmenestret.maze.ADT

final case class GameMap(sideLength: Int, trapsPosition: List[Int], playerPosition: Int, finishPosition: Int)
object GameMap {
  def emptyGameMap(length: Int, trapsPosition: List[Int]): GameMap =
    GameMap(sideLength = length,
            trapsPosition = trapsPosition,
            playerPosition = 0,
            finishPosition = length * length - 1)
}
