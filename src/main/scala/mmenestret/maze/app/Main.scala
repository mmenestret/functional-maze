package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def runGame[F[_]](implicit G: GameLogic[F], P: PlayerInteractions[F], R: Rng[F], M: Monad[F]): F[Unit] = {

    def gameLoop(): StateT[F, GameState, Unit] =
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

    for {
      _          ← P.clearPlayerScreen()
      sideLength ← P.afkForMapSize()
      nbOfTraps  ← P.afkForNumberOfTrap()
      layout     ← P.askForKeyboardLayout()
      _          ← P.clearPlayerScreen()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
      _          ← gameLoop().runA(GameState.emptyGameState(layout, sideLength, trapsList))
    } yield ()

  }

  def program[F[+ _]: Sync]: F[Unit] = {
    PrintAndReadLanternaImpl.initiate[F].flatMap { implicit term ⇒
      implicit val g: GameLogic[F]           = GameLogicImpl[F]
      implicit val rng: Rng[F]               = RngImp[F]
      implicit val pi: PlayerInteractions[F] = PlayerInteractionsImpl[F]
      runGame[F]
    }
  }

  program[IO].unsafeRunSync()

}
