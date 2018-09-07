package mmenestret.maze.app
import cats.Monad
import cats.effect.IO
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def program[Effect[_]: GameLogic: PlayerInteractions: Rng: Monad]: Effect[Unit] = {

    val G: GameLogic[Effect]          = GameLogic[Effect]
    val R: Rng[Effect]                = Rng[Effect]
    val P: PlayerInteractions[Effect] = PlayerInteractions[Effect]

    def gameLoop(gameState: GameState, layout: KeyboardLayout): Effect[Unit] =
      for {
        mapRepresentation ← G.generateMapRepresentation(gameState.map)
        _                 ← P.displayMap(mapRepresentation)
        playerMove        ← P.askPlayerDirection(layout)
        gameState         ← G.updateGameState(gameState, playerMove)
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

  type MyEffect[+A] = IO[A]

  implicit val rng: RngImp[MyEffect]                = new RngImp[MyEffect]                 {}
  implicit val pr: PrintAndReadImpl[MyEffect]       = new PrintAndReadImpl[MyEffect]       {}
  implicit val pi: PlayerInteractionsImpl[MyEffect] = new PlayerInteractionsImpl[MyEffect] {}
  implicit val ga: GameLogicImpl[MyEffect]          = new GameLogicImpl[MyEffect]          {}

  program[MyEffect].unsafeRunSync()

}
