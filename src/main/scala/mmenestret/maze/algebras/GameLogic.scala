package mmenestret.maze.algebras
import mmenestret.maze.ADT._

trait GameLogic[Effect[_]] {
  def generateMapRepresentation(gm: GameMap): Effect[String]
  def computeGameState(gameMap: GameMap, move: Move): Effect[GameState]
  def endMessage(state: Finished): Effect[String]
}
object GameLogic {
  def apply[Effect[_]: GameLogic]: GameLogic[Effect] = implicitly[GameLogic[Effect]]
}
