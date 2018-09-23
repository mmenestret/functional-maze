package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  import GameLogic._
  import PlayerInteractions._
  import Rng._

  def runGame[F[_]: GameLogic: PlayerInteractions: Rng: Monad]: F[Unit] = {

    def gameLoop(): StateT[F, GameState, Unit] =
      for {
        state             ← StateT.get[F, GameState]
        mapRepresentation ← StateT.liftF(generateMapRepresentation(state.map))
        _                 ← StateT.liftF(displayMap(mapRepresentation))
        playerMove        ← StateT.liftF(askPlayerDirection(state.layout))
        gameState         ← StateT.liftF(computeGameState(state, playerMove))
        _ ← gameState match {
          case GameState(_, newMap, OnGoing) ⇒
            StateT.set[F, GameState](gameState.copy(map = newMap): GameState).flatMap(_ ⇒ gameLoop())
          case GameState(_, _, state: Finished) ⇒
            StateT.liftF[F, GameState, Unit](endMessage(state).flatMap(displayEndMessage(_)))
        }
      } yield ()

    for {
      _          ← clearPlayerScreen()
      sideLength ← afkForMapSize()
      nbOfTraps  ← afkForNumberOfTrap()
      layout     ← askForKeyboardLayout()
      _          ← clearPlayerScreen()
      trapsList  ← generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
      map = GameMap.emptyGameMap(sideLength, trapsList)
      _ ← gameLoop().runA(GameState(layout, map, OnGoing))
    } yield ()

  }

  def program[F[+ _]: Sync]: F[Unit] = {
    PrintAndReadLanternaImpl.initiate[F].flatMap { implicit term ⇒
      implicit val g: GameLogic[F]             = GameLogicImpl[F]
      implicit val rng: Rng[F]                 = RngImp[F]
      implicit val pi: PlayerInteractions[F]   = PlayerInteractionsImpl[F]
      runGame[F]
    }
  }

  program[IO].unsafeRunSync()

}
