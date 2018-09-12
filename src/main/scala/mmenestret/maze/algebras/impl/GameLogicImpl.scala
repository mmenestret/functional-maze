package mmenestret.maze.algebras.impl
import cats.MonadError
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras.GameLogic

object GameLogicImpl {

  def apply[Effect[+ _]: MonadError[?[_], Throwable]]: GameLogic[Effect] = new GameLogic[Effect] {
    val M: MonadError[Effect, Throwable] = MonadError[Effect, Throwable]

    def computeNewPosition(gameMap: GameMap, move: Move): Int = {
      val GameMap(maplength, _, currentPosition, _) = gameMap
      move match {
        case Up ⇒
          val unvalidatedPosition = currentPosition - maplength
          if (unvalidatedPosition >= 0) unvalidatedPosition else currentPosition
        case Down ⇒
          val unvalidatedPosition = currentPosition + maplength
          if (unvalidatedPosition < gameMap.sideLength * gameMap.sideLength) unvalidatedPosition else currentPosition
        case m @ _ ⇒
          val unvalidatedPosition = if (m == Left) currentPosition - 1 else currentPosition + 1
          if (unvalidatedPosition / gameMap.sideLength == currentPosition / gameMap.sideLength && unvalidatedPosition >= 0)
            unvalidatedPosition
          else currentPosition
      }
    }

    override def generateMapRepresentation(gameMap: GameMap): Effect[String] = {

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
        initialMapDisplay ← List.fill(mapLength * mapLength)(emptyCellDesign).pure[Effect]

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

    override def computeGameState(gameMap: GameMap, move: Move): Effect[GameState] = {

      def isTrap(pos: Int, gm: GameMap): Boolean = gm.trapsPosition.contains(pos)

      val newPosition = computeNewPosition(gameMap, move)
      val state =
        if (newPosition == gameMap.finishPosition) Won
        else if (isTrap(newPosition, gameMap)) Lost
        else OnGoing
      GameState(gameMap.copy(playerPosition = newPosition), state).pure[Effect]
    }

    override def endMessage(state: Finished): Effect[String] = state match {
      case Lost ⇒ "You lost, you piece of shit !".pure[Effect]
      case Won  ⇒ "You won, lucky bastard !".pure[Effect]
    }
  }

}
