package de.sciss
package mellite
package gui

import de.sciss.synth.proc.{AuralSystem, Sys}
import lucre.stm
import stm.Disposable
import impl.{RecursionFrameImpl => Impl}

object RecursionFrame {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Recursion[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], aural: AuralSystem): RecursionFrame[S] =
    Impl(doc, elem)
}

trait RecursionFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
