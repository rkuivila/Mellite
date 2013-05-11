package de.sciss
package mellite
package gui

import impl.{DocumentCursorsFrameImpl => Impl}
import synth.proc

object DocumentCursorsFrame {
  type S = proc.Confluent
  type D = S#D
  def apply(document: ConfluentDocument)(implicit tx: D#Tx): DocumentCursorsFrame = Impl(document)
}
trait DocumentCursorsFrame /* [S <: Sys[S]] */ {
  def component: desktop.Window
  def document : ConfluentDocument // Document[S]
}