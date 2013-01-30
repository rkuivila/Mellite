package de.sciss.mellite

object InterpreterContext {
  def apply[A](thunk: => A): A = concurrent.stm.atomic(_ => thunk)
}