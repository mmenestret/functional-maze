package mmenestret.maze.app
import cats.Monad
import cats.data.StateT
import cats.effect.{IO, Sync}
import cats.implicits._
import cats.mtl.implicits._
import cats.mtl.MonadState
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def gameLoop[F[_]: Monad](implicit G: GameLogic[F], P: PlayerInteractions[F], S: MonadState[F, GameState]): F[Unit] =
    for {
      state             ← S.get
      mapRepresentation ← G.generateMapRepresentation(state.map)
      _                 ← P.displayMap(mapRepresentation)
      playerMove        ← P.askPlayerDirection(state.layout)
      gameState         ← G.computeGameState(state, playerMove)
      _ ← gameState.status match {
        case OnGoing ⇒
          S.set(gameState).flatMap(_ ⇒ gameLoop[F])
        case status: Finished ⇒
          G.generateEndMessage(status).flatMap(P.displayEndMessage)
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

  def program[F[_]: Sync: MonadState[?[_], GameState]]: F[Unit] = {
    PrintAndReadLanternaImpl.initiate[F].flatMap { implicit term ⇒
      implicit val g: GameLogic[F]           = GameLogicImpl[F]
      implicit val rng: Rng[F]               = RngImp[F]
      implicit val pi: PlayerInteractions[F] = PlayerInteractionsImpl[F]
      for {
        initState ← initiateGame[F]
        _         ← MonadState[F, GameState].set(initState)
        _         ← gameLoop[F]
      } yield ()
    }
  }

  type MyEffect[A] = StateT[IO, GameState, A]
  program[MyEffect].runA(GameState.emptyGameState(Azerty, 10, List())).unsafeRunSync()

}
