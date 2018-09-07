package mmenestret.maze.app
import cats.Monad
import cats.effect.IO
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def program[Effect[_]: GameActions: PlayerInteractions: Rng: Monad]: Effect[Unit] = {

    val G: GameActions[Effect]        = GameActions[Effect]
    val R: Rng[Effect]                = Rng[Effect]
    val P: PlayerInteractions[Effect] = PlayerInteractions[Effect]

    def gameLoop(map: GameMap, layout: KeyboardLayout): Effect[Unit] = {
      for {
        mapAsStr    ← G.generateMapRepresentation(map)
        _           ← P.displayMap(mapAsStr)
        input       ← P.askPlayerKeyboardLayout(layout)
        stateAndMap ← G.updateGameState(map, input)
        _ ← stateAndMap match {
          case (Ongoing, newMap)    ⇒ gameLoop(newMap, layout)
          case (state: Finished, _) ⇒ G.endMessage(state).flatMap(P.displayEndMessage)
        }
      } yield ()
    }

    for {
      sideLength ← P.afkForMapSize()
      nbOfTraps  ← P.afkForNumberOfTrap()
      layout     ← P.askForKeyboardLayout()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
      map = GameMap.emptyGameMap(sideLength, trapsList)
      _ ← gameLoop(map, layout)
    } yield ()

  }

  type MyEffect[+A] = IO[A]

  implicit val rng: RngImp[MyEffect]                = new RngImp[MyEffect]                 {}
  implicit val pr: PrintAndReadImpl[MyEffect]       = new PrintAndReadImpl[MyEffect]       {}
  implicit val pi: PlayerInteractionsImpl[MyEffect] = new PlayerInteractionsImpl[MyEffect] {}
  implicit val ga: GameActionsImpl[MyEffect]        = new GameActionsImpl[MyEffect]        {}

  program[MyEffect].unsafeRunSync()

}
