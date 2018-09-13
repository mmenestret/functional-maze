package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def game[F[_]: GameLogic: PlayerInteractions: Rng: Monad]: F[Unit] = {
    val G: GameLogic[F]          = GameLogic[F]
    val R: Rng[F]                = Rng[F]
    val P: PlayerInteractions[F] = PlayerInteractions[F]

    def gameLoop(layout: KeyboardLayout): StateT[F, GameState, Unit] =
      for {
        state             ← StateT.get[F, GameState]
        mapRepresentation ← StateT.liftF(G.generateMapRepresentation(state.map))
        _                 ← StateT.liftF(P.displayMap(mapRepresentation))
        playerMove        ← StateT.liftF(P.askPlayerDirection(layout))
        gameState         ← StateT.liftF(G.computeGameState(state.map, playerMove))
        _ ← gameState match {
          case GameState(newMap, OnGoing) ⇒
            StateT.set[F, GameState](gameState.copy(newMap): GameState).flatMap(_ ⇒ gameLoop(layout))
          case GameState(_, state: Finished) ⇒
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
      map = GameMap.emptyGameMap(sideLength, trapsList)
      _ ← gameLoop(layout).runA(GameState(map, OnGoing))
    } yield ()

  }

  def program[F[+ _]: Sync]: F[Unit] = {
    PrintAndReadLanternaImpl[F].flatMap { implicit term ⇒
      implicit val g: GameLogic[F]           = GameLogicImpl[F]
      implicit val rng: Rng[F]               = RngImp[F]
      implicit val pi: PlayerInteractions[F] = PlayerInteractionsImpl[F]
      game[F]
    }
  }

  program[IO].unsafeRunSync()

}
