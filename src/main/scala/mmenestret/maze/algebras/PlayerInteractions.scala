package mmenestret.maze.algebras
import mmenestret.maze.ADT.{KeyboardLayout, Move}

trait PlayerInteractions[F[_]] {
  def displayMap(mapAsString: String): F[Unit]
  def displayEndMessage(msg: String): F[Unit]
  def askPlayerDirection(layout: KeyboardLayout): F[Move]
  def afkForMapSize(): F[Int]
  def afkForNumberOfTrap(): F[Int]
  def askForKeyboardLayout(): F[KeyboardLayout]
  def clearPlayerScreen(): F[Unit]
}
object PlayerInteractions {
  def apply[F[_]: PlayerInteractions]: PlayerInteractions[F]                            = implicitly[PlayerInteractions[F]]
  def displayMap[F[_]](mapAsString: String)(implicit F: PlayerInteractions[F]): F[Unit] = F.displayMap(mapAsString)
  def displayEndMessage[F[_]](msg: String)(implicit F: PlayerInteractions[F]): F[Unit]  = F.displayEndMessage(msg)
  def askPlayerDirection[F[_]](layout: KeyboardLayout)(implicit F: PlayerInteractions[F]): F[Move] =
    F.askPlayerDirection(layout)
  def afkForMapSize[F[_]]()(implicit F: PlayerInteractions[F]): F[Int]                   = F.afkForMapSize()
  def afkForNumberOfTrap[F[_]]()(implicit F: PlayerInteractions[F]): F[Int]              = F.afkForNumberOfTrap()
  def askForKeyboardLayout[F[_]]()(implicit F: PlayerInteractions[F]): F[KeyboardLayout] = F.askForKeyboardLayout()
  def clearPlayerScreen[F[_]]()(implicit F: PlayerInteractions[F]): F[Unit]              = F.clearPlayerScreen()
}
