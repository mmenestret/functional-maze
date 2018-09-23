package mmenestret.maze.algebras
import cats.Monad
import cats.mtl.MonadState
import cats.implicits._
import mmenestret.maze.ADT._

trait GameLogic[F[_]] {
  def generateMapRepresentation(gm: GameMap): F[String]
  def computeGameState(gameState: GameState, move: Move): F[GameState]
  def endMessage(state: Finished): F[String]
  def initiateGameState(gameState: GameState)(implicit m: Monad[F]): MonadState[F, GameState] =
    new MonadState[F, GameState] {
      var internalState: GameState                           = gameState
      override val monad: Monad[F]                           = Monad[F]
      override def get: F[GameState]                         = internalState.pure[F]
      override def set(s: GameState): F[Unit]                = { internalState = s }.pure[F]
      override def inspect[A](f: GameState ⇒ A): F[A]        = f(internalState).pure[F]
      override def modify(f: GameState ⇒ GameState): F[Unit] = { internalState = f(internalState) }.pure[F]
    }
}
object GameLogic {
  def apply[F[_]: GameLogic]: GameLogic[F] = implicitly[GameLogic[F]]
}
