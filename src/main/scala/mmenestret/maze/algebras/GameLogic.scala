package mmenestret.maze.algebras
import mmenestret.maze.ADT._

trait GameLogic[F[_]] {
  def generateMapRepresentation(gm: GameMap): F[String]
  def computeGameState(gameMap: GameMap, move: Move): F[GameState]
  def endMessage(state: Finished): F[String]
}
object GameLogic {
  def apply[F[_]: GameLogic]: GameLogic[F] = implicitly[GameLogic[F]]
}
