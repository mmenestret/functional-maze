package mmenestret.maze.app
import cats.effect.IO
import cats.implicits._
import cats.{Applicative, Monad}

import scala.util.Random

object Main extends App {

  final case class GameMap(sideLength: Int, trapsPosition: List[Int], playerPosition: Int, finishPosition: Int)
  object GameMap {
    def emptyGameMap(length: Int, trapsPosition: List[Int]): GameMap =
      GameMap(sideLength = length,
              trapsPosition = trapsPosition,
              playerPosition = 0,
              finishPosition = length * length - 1)
  }

  sealed trait KeyboardLayout {
    def upKey: Char
    def downKey: Char
    def leftKey: Char
    def rightKey: Char
  }
  object Azerty extends KeyboardLayout {
    val upKey: Char    = 'z'
    val downKey: Char  = 's'
    val leftKey: Char  = 'q'
    val rightKey: Char = 'd'

  }
  object Qwerty extends KeyboardLayout {
    def upKey: Char    = 'w'
    def downKey: Char  = 's'
    def leftKey: Char  = 'a'
    def rightKey: Char = 'd'
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

  trait PlayerInteractions[A[_]] {
    def printStr(str: String): A[Unit]
    def getPlayerInput(layout: KeyboardLayout): A[Move]
    def afkForMapSize(): A[Int]
    def afkForNumberOfTrap(): A[Int]
    def askForKeyboardLayout(): A[KeyboardLayout]
  }
  object PlayerInteractions {
    def apply[A[_]: PlayerInteractions]: PlayerInteractions[A] = implicitly[PlayerInteractions[A]]
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
    def updateGameState(gameMap: GameMap, move: Move): A[(GameState, GameMap)]
    def endMessage(state: Finished): A[String]
  }
  object GameDSL {
    def apply[A[_]: GameDSL]: GameDSL[A] = implicitly[GameDSL[A]]
  }

  def program[A[_]: GameDSL: PlayerInteractions: Rng: Monad]: A[Unit] = {

    val G: GameDSL[A]            = GameDSL[A]
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

  implicit val ioGameInterpreter: GameDSL[IO] = new GameDSL[IO] {
    override def generateMapRepresentation(gameMap: GameMap): IO[String] = {
      def lineWithPlayerToStr(playerChar: String)(l: List[String]): String = {
        s"|${l.map(c ⇒ if (c != playerChar) s" $c " else c).mkString("")}|"
      }
      val playerChar = "\\o/"
      val trapChar   = "x"
      val finishChar = "?"
      def lineToStr  = lineWithPlayerToStr(playerChar) _

      val GameMap(mapLength, trapsPosition, currentPosition, finishPosition) = gameMap
      val emptyMap                                                           = List.fill(mapLength * mapLength)(" ")
      for {
        mapWithPlayer ← IO { emptyMap.updated(currentPosition, playerChar).updated(finishPosition, finishChar) }
        mapWithTraps ← trapsPosition.foldLeft(mapWithPlayer.pure[IO])((gameMapTry, trapPosition) ⇒
          gameMapTry.flatMap(gm ⇒ IO { gm.updated(trapPosition, trapChar) }))
        topBorder              = s" ${"_" * (mapLength * 3)} "
        bottomBorder           = s" ${"°" * (mapLength * 3)} "
        (firstLine, rest)      = mapWithTraps.splitAt(mapLength) // First mapLength cells and rest
        (innerLines, lastLine) = rest.splitAt(mapLength * (mapLength - 2))
        first                  = s" ${lineToStr(firstLine).tail}"
        inner: String          = s"${innerLines.grouped(mapLength).map(lineToStr).mkString("\n")}"
        last: String           = s"${lineToStr(lastLine).dropRight(1)} "
      } yield s"$topBorder\n$first\n$inner\n$last\n$bottomBorder"
    }

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

      def isTrap(pos: Int, gm: GameMap): Boolean = gm.trapsPosition.contains(pos)

      val np = computeNewPosition(gameMap, move)
      val state =
        if (np == gameMap.finishPosition) Won
        else if (isTrap(np, gameMap)) Lost
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

  implicit val playerInteractions: PlayerInteractions[IO] = new PlayerInteractions[IO] {
    override def getPlayerInput(layout: KeyboardLayout): IO[Move] = {
      IO { scala.io.StdIn.readChar() }
        .flatMap {
          case k if k == layout.upKey    ⇒ Up.pure[IO]
          case k if k == layout.downKey  ⇒ Down.pure[IO]
          case k if k == layout.leftKey  ⇒ Left.pure[IO]
          case k if k == layout.rightKey ⇒ Right.pure[IO]
          case _                         ⇒ getPlayerInput(layout)
        }
        .recoverWith { case _ ⇒ getPlayerInput(layout) }
    }

    override def printStr(str: String): IO[Unit] = IO(println(str))

    override def afkForMapSize(): IO[Int] =
      (for {
        _        ← printStr("What's the map side's size you want to play on, noob ?")
        sideSize ← IO(scala.io.StdIn.readInt())
      } yield sideSize).recoverWith { case _ ⇒ afkForMapSize() }

    override def afkForNumberOfTrap(): IO[Int] =
      (for {
        _             ← printStr("How many traps ?")
        numberOfTraps ← IO(scala.io.StdIn.readInt())
      } yield numberOfTraps).recoverWith { case _ ⇒ afkForNumberOfTrap() }

    override def askForKeyboardLayout(): IO[KeyboardLayout] =
      (for {
        _     <- printStr("(A)zerty or (Q)werty ? (A or Q for the idiots who didn't get it...)")
        input ← IO(scala.io.StdIn.readChar().toLower)
        layout = input match {
          case 'a' ⇒ Azerty
          case 'q' ⇒ Qwerty
        }
      } yield layout).recoverWith { case _ => askForKeyboardLayout() }
  }

  program[IO].unsafeRunSync()

}
