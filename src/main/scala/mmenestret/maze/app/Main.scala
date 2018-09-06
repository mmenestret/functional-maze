package mmenestret.maze.app
import cats.effect.IO
import cats._
import cats.implicits._
import cats.{Applicative, Monad}

import scala.util.Random

object Main extends App {

  final case class GameMap(sideLength: Int, holesPosition: List[Int], playerPosition: Int, finishPosition: Int)
  object GameMap {
    def emptyGameMap(length: Int, holesPosition: List[Int]): GameMap =
      GameMap(sideLength = length,
              holesPosition = holesPosition,
              playerPosition = 0,
              finishPosition = length * length - 1)
  }

  sealed trait Move
  case object Up    extends Move
  case object Down  extends Move
  case object Left  extends Move
  case object Right extends Move

  sealed trait GameState
  case object Ongoing   extends GameState
  sealed trait Finished extends GameState
  case object Lost      extends Finished
  case object Won       extends Finished

  trait ConsoleIO[A[_]] {
    def printStr(str: String): A[Unit]
  }
  object ConsoleIO {
    def apply[A[_]: ConsoleIO]: ConsoleIO[A] = implicitly[ConsoleIO[A]]
  }

  trait Rng[A[_]] {
    def generateRngBetween(low: Int, high: Int): A[Int]
    def generateNRngBetween(n: Int)(low: Int, high: Int)(implicit app: Applicative[A]): A[List[Int]] =
      (0 to n).toList.traverse(_ ⇒ generateRngBetween(low, high))
  }
  object Rng {
    def apply[A[_]: Rng]: Rng[A] = implicitly[Rng[A]]
  }

  trait GameDSL[A[_]] {
    def generateMapRepresentation(gm: GameMap): A[String]
    def getPlayerInput: A[Move]
    def updateGameState(gameMap: GameMap, move: Move): A[(GameState, GameMap)]
    def endMessage(state: Finished): A[String]
  }
  object GameDSL {
    def apply[A[_]: GameDSL]: GameDSL[A] = implicitly[GameDSL[A]]
  }

  def program[A[_]: GameDSL: ConsoleIO: Rng: Monad](sideLength: Int, nbOfHoles: Int): A[Unit] = {

    val G: GameDSL[A]   = GameDSL[A]
    val R: Rng[A]       = Rng[A]
    val C: ConsoleIO[A] = ConsoleIO[A]

    def gameLoop(map: GameMap): A[Unit] = {
      for {
        mapAsStr    ← G.generateMapRepresentation(map)
        _           ← C.printStr(mapAsStr)
        input       ← G.getPlayerInput
        stateAndMap ← G.updateGameState(map, input)
        _ ← stateAndMap match {
          case (Ongoing, newMap)    ⇒ gameLoop(newMap)
          case (state: Finished, _) ⇒ G.endMessage(state).flatMap(C.printStr)
        }
      } yield ()
    }

    for {
      holesList ← R.generateNRngBetween(nbOfHoles)(1, sideLength * sideLength - 1)
      map = GameMap.emptyGameMap(sideLength, holesList)
      _ ← gameLoop(map)
    } yield ()

  }

  implicit val ioGameInterpreter: GameDSL[IO] = new GameDSL[IO] {
    override def generateMapRepresentation(gameMap: GameMap): IO[String] = {
      def lineWithPlayerToStr(playerChar: String)(l: List[String]): String = {
        s"|${l.map(c ⇒ if (c != playerChar) s" $c " else c).mkString("")}|"
      }
      val playerChar = "\\o/"
      val holeChar   = "x"
      val finishChar = "?"
      def lineToStr  = lineWithPlayerToStr(playerChar) _

      val GameMap(mapLength, holesPosition, currentPosition, finishPosition) = gameMap
      val emptyMap                                                           = List.fill(mapLength * mapLength)(" ")
      for {
        mapWithPlayer ← IO { emptyMap.updated(currentPosition, playerChar).updated(finishPosition, finishChar) }
        mapWithHoles ← holesPosition.foldLeft(mapWithPlayer.pure[IO])((gameMapTry, holePosition) ⇒
          gameMapTry.flatMap(gm ⇒ IO { gm.updated(holePosition, holeChar) }))
        topBorder              = s" ${"_" * (mapLength * 3)} "
        bottomBorder           = s" ${"`" * (mapLength * 3)} "
        (firstLine, rest)      = mapWithHoles.splitAt(mapLength) // First mapLength cells and rest
        (innerLines, lastLine) = rest.splitAt(mapLength * (mapLength - 2))
        first                  = s" ${lineToStr(firstLine).tail}"
        inner: String          = s"${innerLines.grouped(mapLength).map(lineToStr).mkString("\n")}"
        last: String           = s"${lineToStr(lastLine).dropRight(1)} "
      } yield s"$topBorder\n$first\n$inner\n$last\n$bottomBorder"
    }

    override def getPlayerInput: IO[Move] =
      IO { scala.io.StdIn.readChar() }
        .flatMap {
          case 'z' ⇒ Up.pure[IO]
          case 's' ⇒ Down.pure[IO]
          case 'q' ⇒ Left.pure[IO]
          case 'd' ⇒ Right.pure[IO]
          case _   ⇒ getPlayerInput
        }
        .recoverWith { case _ ⇒ getPlayerInput }

    def computeNewPosition(gameMap: GameMap, move: Move): Int = {
      val GameMap(maplength, _, currentPosition, _) = gameMap
      move match {
        case Main.Up ⇒
          val unvalidatedPosition = currentPosition - maplength
          if (unvalidatedPosition >= 0) unvalidatedPosition else currentPosition
        case Main.Down ⇒
          val unvalidatedPosition = currentPosition + maplength
          if (unvalidatedPosition < gameMap.sideLength * gameMap.sideLength) unvalidatedPosition else currentPosition
        case m @ _ ⇒
          val unvalidatedPosition = if (m == Left) currentPosition - 1 else currentPosition + 1
          if (unvalidatedPosition / gameMap.sideLength == currentPosition / gameMap.sideLength && unvalidatedPosition >= 0)
            unvalidatedPosition
          else currentPosition
      }
    }

    override def updateGameState(gameMap: GameMap, move: Move): IO[(GameState, GameMap)] = {

      def isHole(pos: Int, gm: GameMap): Boolean = gm.holesPosition.contains(pos)

      val np = computeNewPosition(gameMap, move)
      val state =
        if (np == gameMap.finishPosition) Won
        else if (isHole(np, gameMap)) Lost
        else Ongoing
      (state, gameMap.copy(playerPosition = np)).pure[IO]
    }

    override def endMessage(state: Finished): IO[String] = state match {
      case Lost ⇒ "You lost, you piece of shit !".pure[IO]
      case Won  ⇒ "You won, lucky bastard !".pure[IO]
    }

  }

  implicit val rng: Rng[IO] = new Rng[IO] {
    override def generateRngBetween(low: Int, high: Int): IO[Int] = IO(Random.nextInt(high - low) + low)
  }

  implicit val consoleIO: ConsoleIO[IO] = new ConsoleIO[IO] {
    override def printStr(str: String): IO[Unit] = IO(println(str))
  }

  program[IO](10, 30).unsafeRunSync()

}
