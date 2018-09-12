package mmenestret.maze.app
import cats.Monad
import cats.effect.{IO, Sync}
import cats.implicits._
import mmenestret.maze.ADT._
import mmenestret.maze.algebras._
import mmenestret.maze.algebras.impl._

object Main extends App {

  def game[Effect[_]: GameLogic: PlayerInteractions: Rng: Monad]: Effect[Unit] = {

    val G: GameLogic[Effect]          = GameLogic[Effect]
    val R: Rng[Effect]                = Rng[Effect]
    val P: PlayerInteractions[Effect] = PlayerInteractions[Effect]

    def gameLoop(gameState: GameState, layout: KeyboardLayout): Effect[Unit] =
      for {
        mapRepresentation ← G.generateMapRepresentation(gameState.map)
        _                 ← P.displayMap(mapRepresentation)
        playerMove        ← P.askPlayerDirection(layout)
        gameState         ← G.updateGameState(gameState.map, playerMove)
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

  def program[Effect[+ _]: Sync] =
    PrintAndReadLanternaImpl.create[Effect].flatMap { implicit term ⇒
      implicit val g: GameLogic[Effect]           = new GameLogicImpl[Effect]
      implicit val rng: Rng[Effect]               = new RngImp[Effect]
      implicit val pi: PlayerInteractions[Effect] = new PlayerInteractionsImpl[Effect]
      game[Effect]
    }

  type MyEffect[A] = IO[A]

  program[IO].unsafeRunSync()

}

object Test extends App {

  PrintAndReadLanternaImpl.create[IO].flatMap(t ⇒ t.readStr).unsafeRunSync()

  /*/*
    If we got a terminal emulator (probably Swing) then we are currently looking at an empty terminal emulator
    window at this point. If the code ran in another terminal emulator (putty, gnome-terminal, konsole, etc) by
    invoking java manually, there is yet no changes to the content.
 */

  /*
    Let's print some text, this has the same as calling System.out.println("Hello");
 */
  terminal.putCharacter('H');
  terminal.putCharacter('e');
  terminal.putCharacter('l');
  terminal.putCharacter('l');
  terminal.putCharacter('o');
  terminal.putCharacter('\n');
  terminal.flush();
  /*
    Notice the flush() call above; it is necessary to finish off terminal output operations with a call to
    flush() both in the case of native terminal and the bundled terminal emulators. Lanterna's Unix terminal
    doesn't buffer the output by itself but one can assume the underlying I/O layer does. In the case of the
    terminal emulators bundled in lanterna, the flush call will signal a repaint to the underlying UI component.
 */
  Thread.sleep(2000);

  /*
    At this point the cursor should be on start of the next line, immediately after the Hello that was just
    printed. Let's move the cursor to a new position, relative to the current position. Notice we still need to
    call flush() to ensure the change is immediately visible (i.e. the user can see the text cursor moved to the
    new position).
    One thing to notice here is that if you are running this in a 'proper' terminal and the cursor position is
    at the bottom line, it won't actually move the text up. Attempts at setting the cursor position outside the
    terminal bounds are usually rounded to the first/last column/row. If you run into this, please clear the
    terminal content so the cursor is at the top again before running this code.
 */
  val startPosition: TerminalPosition = terminal.getCursorPosition();
  terminal.setCursorPosition(startPosition.withRelativeColumn(3).withRelativeRow(2));
  terminal.flush();
  Thread.sleep(2000);

  /*
    Let's continue by changing the color of the text printed. This doesn't change any currently existing text,
    it will only take effect on whatever we print after this.
 */
  terminal.setBackgroundColor(TextColor.ANSI.BLUE);
  terminal.setForegroundColor(TextColor.ANSI.YELLOW);

  /*
    Now print text with these new colors
 */
  terminal.putCharacter('Y');
  terminal.putCharacter('e');
  terminal.putCharacter('l');
  terminal.putCharacter('l');
  terminal.putCharacter('o');
  terminal.putCharacter('w');
  terminal.putCharacter(' ');
  terminal.putCharacter('o');
  terminal.putCharacter('n');
  terminal.putCharacter(' ');
  terminal.putCharacter('b');
  terminal.putCharacter('l');
  terminal.putCharacter('u');
  terminal.putCharacter('e');
  terminal.flush();
  Thread.sleep(2000);

  /*
    In addition to colors, most terminals supports some sort of style that can be selectively enabled. The most
    common one is bold mode, which on many terminal implementations (emulators and otherwise) is not actually
    using bold text at all but rather shifts the tint of the foreground color so it stands out a bit. Let's
    print the same text as above in bold mode to compare.
    Notice that startPosition has the same value as when we retrieved it with getTerminalSize(), the
    TerminalPosition class is immutable and calling the with* methods will return a copy. So the following
    setCursorPosition(..) call will put us exactly one row below the previous row.
 */
  terminal.setCursorPosition(startPosition.withRelativeColumn(3).withRelativeRow(3));
  terminal.flush();
  Thread.sleep(2000);
  terminal.enableSGR(SGR.BOLD);
  terminal.putCharacter('Y');
  terminal.putCharacter('e');
  terminal.putCharacter('l');
  terminal.putCharacter('l');
  terminal.putCharacter('o');
  terminal.putCharacter('w');
  terminal.putCharacter(' ');
  terminal.putCharacter('o');
  terminal.putCharacter('n');
  terminal.putCharacter(' ');
  terminal.putCharacter('b');
  terminal.putCharacter('l');
  terminal.putCharacter('u');
  terminal.putCharacter('e');
  terminal.flush();
  Thread.sleep(2000);

  /*
    Ok, that's enough for now. Let's reset colors and SGR modifiers and move down one more line
 */
  terminal.resetColorAndSGR();
  terminal.setCursorPosition(terminal.getCursorPosition().withColumn(0).withRelativeRow(1));
  terminal.putCharacter('D');
  terminal.putCharacter('o');
  terminal.putCharacter('n');
  terminal.putCharacter('e');
  terminal.putCharacter('\n');
  terminal.flush();

  Thread.sleep(2000);

  /*
    Beep and exit
 */
  terminal.bell();
  terminal.flush();
  Thread.sleep(200);*/

}
