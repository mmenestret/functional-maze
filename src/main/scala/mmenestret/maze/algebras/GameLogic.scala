package mmenestret.maze.algebras
import mmenestret.maze.ADT._

trait GameLogic[F[_]] {
  def generateMapRepresentation(gm: GameMap): F[String]
  def computeGameState(gameState: GameState, move: Move): F[GameState]
  def generateEndMessage(state: Finished): F[String]
}
object GameLogic {
  def apply[F[_]: GameLogic]: GameLogic[F] = implicitly[GameLogic[F]]
}
