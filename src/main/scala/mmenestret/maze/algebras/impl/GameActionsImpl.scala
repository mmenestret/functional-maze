package mmenestret.maze.algebras.impl
import cats.Monad
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras.GameActions

class GameActionsImpl[Effect[+ _]: Monad] extends GameActions[Effect] {

  val M: Monad[Effect] = Monad[Effect]

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

    def lineWithPlayerToStr(playerChar: String)(l: List[String]): String = {
      s"|${l.map(c ⇒ if (c != playerChar) s" $c " else c).mkString("")}|"
    }
    val playerChar                       = "\\o/"
    val trapChar                         = "x"
    val finishChar                       = "?"
    def lineToStr: List[String] ⇒ String = lineWithPlayerToStr(playerChar)

    val GameMap(mapLength, trapsPosition, currentPosition, finishPosition) = gameMap
    val emptyMap                                                           = List.fill(mapLength * mapLength)(" ")

    for {
      mapWithPlayer ← emptyMap
        .updated(currentPosition, playerChar)
        .updated(finishPosition, finishChar)
        .pure[Effect]
      mapWithTraps ← trapsPosition.foldLeft(mapWithPlayer.pure[Effect])((gameMapTry, trapPosition) ⇒
        gameMapTry.flatMap(gm ⇒ gm.updated(trapPosition, trapChar).pure[Effect]))
      topBorder              = s" ${"_" * (mapLength * 3)} "
      bottomBorder           = s" ${"°" * (mapLength * 3)} "
      (firstLine, rest)      = mapWithTraps.splitAt(mapLength) // First mapLength cells and rest
      (innerLines, lastLine) = rest.splitAt(mapLength * (mapLength - 2))
      first                  = s" ${lineToStr(firstLine).tail}"
      inner: String          = s"${innerLines.grouped(mapLength).map(lineToStr).mkString("\n")}"
      last: String           = s"${lineToStr(lastLine).dropRight(1)} "
    } yield s"$topBorder\n$first\n$inner\n$last\n$bottomBorder"
  }

  override def updateGameState(gameMap: GameMap, move: Move): Effect[(GameState, GameMap)] = {

    def isTrap(pos: Int, gm: GameMap): Boolean = gm.trapsPosition.contains(pos)

    val np = computeNewPosition(gameMap, move)
    val state =
      if (np == gameMap.finishPosition) Won
      else if (isTrap(np, gameMap)) Lost
      else Ongoing
    (state, gameMap.copy(playerPosition = np)).pure[Effect]
  }

  override def endMessage(state: Finished): Effect[String] = state match {
    case Lost ⇒ "You lost, you piece of shit !".pure[Effect]
    case Won  ⇒ "You won, lucky bastard !".pure[Effect]
  }

}
