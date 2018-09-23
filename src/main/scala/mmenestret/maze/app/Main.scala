package mmenestret.maze.app
import cats.Monad
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.mtl.MonadState
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def gameLoop[F[_]: Monad]()(implicit G: GameLogic[F],
                              P: PlayerInteractions[F],
                              S: MonadState[F, GameState]): F[Unit] =
    for {
      state             ← S.get
      mapRepresentation ← G.generateMapRepresentation(state.map)
      _                 ← P.displayMap(mapRepresentation)
      playerMove        ← P.askPlayerDirection(state.layout)
      gameState         ← G.computeGameState(state, playerMove)
      _ ← gameState match {
        case GameState(_, newMap, OnGoing) ⇒
          S.set(gameState.copy(map = newMap): GameState).flatMap(_ ⇒ gameLoop[F]())
        case GameState(_, _, state: Finished) ⇒
          G.endMessage(state).flatMap(P.displayEndMessage)
      }
    } yield ()

  def initiateGame[F[_]: Monad](implicit P: PlayerInteractions[F],
                                R: Rng[F],
                                G: GameLogic[F]): F[MonadState[F, GameState]] =
    for {
      _          ← P.clearPlayerScreen()
      sideLength ← P.afkForMapSize()
      nbOfTraps  ← P.afkForNumberOfTrap()
      layout     ← P.askForKeyboardLayout()
      _          ← P.clearPlayerScreen()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
    } yield G.initiateGameState(GameState.emptyGameState(layout, sideLength, trapsList))

  def program[F[+ _]: Sync]: F[Unit] = {
    PrintAndReadLanternaImpl.initiate[F].flatMap { implicit term ⇒
      implicit val g: GameLogic[F]           = GameLogicImpl[F]
      implicit val rng: Rng[F]               = RngImp[F]
      implicit val pi: PlayerInteractions[F] = PlayerInteractionsImpl[F]
      initiateGame[F].flatMap { implicit initialState ⇒
        gameLoop[F]()
      }
    }
  }

  program[IO].unsafeRunSync()

}
