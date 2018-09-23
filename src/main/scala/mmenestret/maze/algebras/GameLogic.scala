package mmenestret.maze.algebras
import mmenestret.maze.ADT._

trait GameLogic[F[_]] {
  def generateMapRepresentation(gm: GameMap): F[String]
  def computeGameState(gameState: GameState, move: Move): F[GameState]
  def endMessage(state: Finished): F[String]
}
object GameLogic {
  def apply[F[_]: GameLogic]: GameLogic[F] = implicitly
  def generateMapRepresentation[F[_]](gm: GameMap)(implicit F: GameLogic[F]): F[String] =
    F.generateMapRepresentation(gm)
  def computeGameState[F[_]](gameState: GameState, move: Move)(implicit F: GameLogic[F]): F[GameState] =
    F.computeGameState(gameState, move)
  def endMessage[F[_]](state: Finished)(implicit F: GameLogic[F]): F[String] = F.endMessage(state)
}
