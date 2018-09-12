package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def game[Effect[_]: GameLogic: PlayerInteractions: Rng: Monad]: Effect[Unit] = {
    val G: GameLogic[Effect]          = GameLogic[Effect]
    val R: Rng[Effect]                = Rng[Effect]
    val P: PlayerInteractions[Effect] = PlayerInteractions[Effect]

    def gameLoop(layout: KeyboardLayout): StateT[Effect, GameState, Unit] =
      for {
        state             ← StateT.get[Effect, GameState]
        mapRepresentation ← StateT.liftF(G.generateMapRepresentation(state.map))
        _                 ← StateT.liftF(P.displayMap(mapRepresentation))
        playerMove        ← StateT.liftF(P.askPlayerDirection(layout))
        gameState         ← StateT.liftF(G.computeGameState(state.map, playerMove))
        _ ← gameState match {
          case GameState(newMap, OnGoing) ⇒
            StateT.set[Effect, GameState](gameState.copy(newMap): GameState).flatMap(_ ⇒ gameLoop(layout))
          case GameState(_, state: Finished) ⇒
            StateT.liftF[Effect, GameState, Unit](G.endMessage(state).flatMap(P.displayEndMessage))
        }
      } yield ()

    for {
      _          ← P.clearPlayerScreen()
      sideLength ← P.afkForMapSize()
      nbOfTraps  ← P.afkForNumberOfTrap()
      layout     ← P.askForKeyboardLayout()
      _          ← P.clearPlayerScreen()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
      map = GameMap.emptyGameMap(sideLength, trapsList)
      _ ← gameLoop(layout).runA(GameState(map, OnGoing))
    } yield ()

  }

  def program[Effect[+ _]: Sync]: Effect[Unit] = {
    PrintAndReadLanternaImpl[Effect].flatMap { implicit term ⇒
      implicit val g: GameLogic[Effect]           = GameLogicImpl[Effect]
      implicit val rng: Rng[Effect]               = RngImp[Effect]
      implicit val pi: PlayerInteractions[Effect] = PlayerInteractionsImpl[Effect]
      game[Effect]
    }
  }

  type MyEffect[A] = IO[A]

  program[IO].unsafeRunSync()

}
