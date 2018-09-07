package mmenestret.maze.algebras
import mmenestret.maze.ADT._

trait GameActions[Effect[_]] {
  def generateMapRepresentation(gm: GameMap): Effect[String]
  def updateGameState(gameMap: GameMap, move: Move): Effect[(GameState, GameMap)]
  def endMessage(state: Finished): Effect[String]
}
object GameActions {
  def apply[Effect[_]: GameActions]: GameActions[Effect] = implicitly[GameActions[Effect]]
}
