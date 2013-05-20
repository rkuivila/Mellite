package de.sciss
package mellite
package gui

import de.sciss.synth.proc.{AuralSystem, Sys}
import lucre.stm
import stm.Disposable
import impl.{AudioFileFrameImpl => Impl}

object AudioFileFrame {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.AudioGrapheme[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], aural: AuralSystem): AudioFileFrame[S] =
    Impl(doc, elem)
}

trait AudioFileFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
