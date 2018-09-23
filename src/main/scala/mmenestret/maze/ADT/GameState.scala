package mmenestret.maze.ADT

final case class GameState(layout: KeyboardLayout, map: GameMap, status: GameStatus)
object GameState {
  def emptyGameState(layout: KeyboardLayout, length: Int, trapsPosition: List[Int]) =
    GameState(layout, GameMap.emptyGameMap(length, trapsPosition), OnGoing)
}
