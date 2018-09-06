package mmenestret.maze.algebras
import mmenestret.maze.ADT._

trait GameActions[A[_]] {
  def generateMapRepresentation(gm: GameMap): A[String]
  def updateGameState(gameMap: GameMap, move: Move): A[(GameState, GameMap)]
  def endMessage(state: Finished): A[String]
}
object GameActions {
  def apply[A[_]: GameActions]: GameActions[A] = implicitly[GameActions[A]]
}
