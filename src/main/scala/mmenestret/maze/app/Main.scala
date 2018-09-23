package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def gameLoop[F[_]: Monad](implicit G: GameLogic[F], P: PlayerInteractions[F]): StateT[F, GameState, Unit] = {
    implicit class Lifter[A](val fa: F[A]) { def lift: StateT[F, GameState, A] = StateT.liftF(fa) }
    for {
      state             ← StateT.get[F, GameState]
      mapRepresentation ← G.generateMapRepresentation(state.map).lift
      _                 ← P.displayMap(mapRepresentation).lift
      playerMove        ← P.askPlayerDirection(state.layout).lift
      gameState         ← G.computeGameState(state, playerMove).lift
      _ ← gameState.status match {
        case OnGoing ⇒
          StateT.set[F, GameState](gameState).flatMap(_ ⇒ gameLoop[F])
        case status: Finished ⇒
          G.generateEndMessage(status).flatMap(P.displayEndMessage).lift
      }
    } yield ()
  }

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
        _            ← gameLoop[F].runA(initialState)
      } yield ()
    }
  }

  program[IO].unsafeRunSync()

}
