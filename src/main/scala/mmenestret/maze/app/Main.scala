package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def gameLoop[F[_]: Monad]()(implicit G: GameLogic[F], P: PlayerInteractions[F]): StateT[F, GameState, Unit] =
    for {
      state             ← StateT.get[F, GameState]
      mapRepresentation ← StateT.liftF(G.generateMapRepresentation(state.map))
      _                 ← StateT.liftF(P.displayMap(mapRepresentation))
      playerMove        ← StateT.liftF(P.askPlayerDirection(state.layout))
      gameState         ← StateT.liftF(G.computeGameState(state, playerMove))
      _ ← gameState match {
        case GameState(_, newMap, OnGoing) ⇒
          StateT.set[F, GameState](gameState.copy(map = newMap): GameState).flatMap(_ ⇒ gameLoop())
        case GameState(_, _, state: Finished) ⇒
          StateT.liftF[F, GameState, Unit](G.endMessage(state).flatMap(P.displayEndMessage))
      }
    } yield ()

  def initiateGame[F[_]: Monad](implicit P: PlayerInteractions[F], R: Rng[F]): F[GameState] =
    for {
      _          ← P.clearPlayerScreen()
      sideLength ← P.afkForMapSize()
      nbOfTraps  ← P.afkForNumberOfTrap()
      layout     ← P.askForKeyboardLayout()
      _          ← P.clearPlayerScreen()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
    } yield GameState.emptyGameState(layout, sideLength, trapsList)

  def program[F[+ _]: Sync]: F[Unit] = {
    PrintAndReadLanternaImpl.initiate[F].flatMap { implicit term ⇒
      implicit val g: GameLogic[F]           = GameLogicImpl[F]
      implicit val rng: Rng[F]               = RngImp[F]
      implicit val pi: PlayerInteractions[F] = PlayerInteractionsImpl[F]
      for {
        initialState ← initiateGame[F]
        _            ← gameLoop[F]().runA(initialState)
      } yield ()
    }
  }

  program[IO].unsafeRunSync()

}
