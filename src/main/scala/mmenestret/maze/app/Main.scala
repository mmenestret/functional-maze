package mmenestret.maze.app
import cats.Monad
import cats.effect.IO
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def program[A[_]: GameActions: PlayerInteractions: Rng: Monad]: A[Unit] = {

    val G: GameActions[A]        = GameActions[A]
    val R: Rng[A]                = Rng[A]
    val C: PlayerInteractions[A] = PlayerInteractions[A]

    def gameLoop(map: GameMap, layout: KeyboardLayout): A[Unit] = {
      for {
        mapAsStr    ← G.generateMapRepresentation(map)
        _           ← C.printStr(mapAsStr)
        input       ← C.getPlayerInput(layout)
        stateAndMap ← G.updateGameState(map, input)
        _ ← stateAndMap match {
          case (Ongoing, newMap)    ⇒ gameLoop(newMap, layout)
          case (state: Finished, _) ⇒ G.endMessage(state).flatMap(C.printStr)
        }
      } yield ()
    }

    for {
      sideLength ← C.afkForMapSize()
      nbOfTraps  ← C.afkForNumberOfTrap()
      layout     ← C.askForKeyboardLayout()
      trapsList  ← R.generateNRngBetween(nbOfTraps)(1, sideLength * sideLength - 1)
      map = GameMap.emptyGameMap(sideLength, trapsList)
      _ ← gameLoop(map, layout)
    } yield ()

  }

  implicit val rng = new RngImp                 {}
  implicit val pi  = new PlayerInteractionsImpl {}
  implicit val ga  = new GameActionsImpl        {}
  program[IO].unsafeRunSync()

}
