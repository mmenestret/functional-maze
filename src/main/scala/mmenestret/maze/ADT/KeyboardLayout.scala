package mmenestret.maze.ADT

sealed abstract class KeyboardLayout {
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
