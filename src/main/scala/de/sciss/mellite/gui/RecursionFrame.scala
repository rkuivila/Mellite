package de.sciss
package mellite
package gui

import synth.proc.Sys
import lucre.stm
import stm.Disposable
import impl.{RecursionFrameImpl => Impl}

object RecursionFrame {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Recursion[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): RecursionFrame[S] =
    Impl(doc, elem)
}

trait RecursionFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
