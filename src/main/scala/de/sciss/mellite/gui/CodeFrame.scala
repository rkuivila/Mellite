package de.sciss
package mellite
package gui

import synth.proc.Sys
import lucre.stm
import stm.Disposable
import impl.interpreter.{CodeFrameImpl => Impl}

object CodeFrame {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Code[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): CodeFrame[S] =
    Impl(doc, elem)
}

trait CodeFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
