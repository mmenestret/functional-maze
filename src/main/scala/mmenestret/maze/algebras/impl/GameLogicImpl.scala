package mmenestret.maze.algebras.impl
import cats.MonadError
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras.GameLogic

object GameLogicImpl {

  def apply[F[_]](implicit M: MonadError[F, Throwable]): GameLogic[F] = new GameLogic[F] {

    def computeNewPosition(sideLength: Int, currentPosition: Int, move: Move): Int = {
      move match {
        case Up ⇒
          val unvalidatedPosition = currentPosition - sideLength
          if (unvalidatedPosition >= 0) unvalidatedPosition else currentPosition
        case Down ⇒
          val unvalidatedPosition = currentPosition + sideLength
          if (unvalidatedPosition < sideLength * sideLength) unvalidatedPosition else currentPosition
        case m @ _ ⇒
          val unvalidatedPosition = if (m == Left) currentPosition - 1 else currentPosition + 1
          val onTheSameLine       = (unvalidatedPosition / sideLength) == (currentPosition / sideLength)
          if (onTheSameLine && unvalidatedPosition >= 0)
            unvalidatedPosition
          else currentPosition
      }
    }

    override def generateMapRepresentation(gameMap: GameMap): F[String] = {

      val GameMap(mapLength, trapsPosition, currentPosition, finishPosition) = gameMap

      val playerDesign       = "\\o/"
      val trapDesign         = "x"
      val finishDesign       = "?"
      val emptyCellDesign    = " "
      val topBorderDesign    = s" ${"_" * (mapLength * 3)} "
      val bottomBorderDesign = s" ${"°" * (mapLength * 3)} "

      def lineToStrWithPlayer(playerDesign: String)(l: List[String]): String =
        s"|${l.map(c ⇒ if (c != playerDesign) s" $c " else c).mkString("")}|"
      def lineToStr: List[String] ⇒ String = lineToStrWithPlayer(playerDesign)

      for {
        initialMapDisplay ← List.fill(mapLength * mapLength)(emptyCellDesign).pure[F]

        // Unsafe Operations
        finalMapDisplay ← M.catchNonFatal {
          val withPlayerAndFinish = initialMapDisplay
            .updated(currentPosition, playerDesign)
            .updated(finishPosition, finishDesign)
          trapsPosition.foldLeft(withPlayerAndFinish)((gm, trapPosition) ⇒ gm.updated(trapPosition, trapDesign))
        }

        // Safe Operations
        (firstLine, rest)      = finalMapDisplay.splitAt(mapLength) // First mapLength cells and rest
        (innerLines, lastLine) = rest.splitAt(mapLength * (mapLength - 2))
        firstLineWithBorder    = lineToStr(firstLine)

        // Unsafe Operation
        first ← M.catchNonFatal(firstLineWithBorder.tail).map(" " + _)

        // Safe Operations
        inner: String = s"${innerLines.grouped(mapLength).map(lineToStr).mkString("\n")}"
        last: String  = s"${lineToStr(lastLine).dropRight(1)} "

      } yield s"$topBorderDesign\n$first\n$inner\n$last\n$bottomBorderDesign"
    }

    override def computeGameState(gameState: GameState, move: Move): F[GameState] = {

      def isTrap(pos: Int): Boolean = gameState.map.trapsPosition.contains(pos)

      val newPosition = computeNewPosition(gameState.map.sideLength, gameState.map.playerPosition, move)
      val newMap      = gameState.map.copy(playerPosition = newPosition)
      val newStatus =
        if (newPosition == gameState.map.finishPosition) Won
        else if (isTrap(newPosition)) Lost
        else OnGoing
      gameState.copy(map = newMap, status = newStatus).pure[F]
    }

    override def generateEndMessage(state: Finished): F[String] = state match {
      case Lost ⇒ "You lost, noob !".pure[F]
      case Won  ⇒ "You won, pure luck !".pure[F]
    }
  }

}
