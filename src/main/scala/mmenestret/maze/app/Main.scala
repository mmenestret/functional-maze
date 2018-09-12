package mmenestret.maze.app
import cats.Monad
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

    def gameLoop(gameState: GameState, layout: KeyboardLayout): Effect[Unit] =
      for {
        mapRepresentation ← G.generateMapRepresentation(gameState.map)
        _                 ← P.displayMap(mapRepresentation)
        playerMove        ← P.askPlayerDirection(layout)
        gameState         ← G.updateGameState(gameState.map, playerMove)
        _ ← gameState match {
          case GameState(newMap, OnGoing)    ⇒ gameLoop(gameState.copy(newMap), layout)
          case GameState(_, state: Finished) ⇒ G.endMessage(state).flatMap(P.displayEndMessage)
        }
      } yield ()

    for {
      sideLength ← P.afkForMapSize()
      nbOfTraps  ← P.afkForNumberOfTrap()
      layout     ← P.askForKeyboardLayout()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
      map = GameMap.emptyGameMap(sideLength, trapsList)
      _ ← gameLoop(GameState(map, OnGoing), layout)
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
