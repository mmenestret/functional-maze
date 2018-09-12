package mmenestret.maze.ADT

sealed abstract class KeyboardLayout
object Azerty extends KeyboardLayout
object Qwerty extends KeyboardLayout

final case class KeyboardKeys(up: Char, down: Char, left: Char, right: Char)

object KeyboardLayout {
  def keys(keyboardLayout: KeyboardLayout): KeyboardKeys = keyboardLayout match {
    case Azerty ⇒ KeyboardKeys(up = 'z', down = 's', left = 'q', right = 'd')
    case Qwerty ⇒ KeyboardKeys(up = 'w', down = 's', left = 'a', right = 'd')
  }
}
