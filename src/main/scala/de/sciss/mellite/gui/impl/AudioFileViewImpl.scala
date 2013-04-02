package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import de.sciss.lucre.stm
import Element.AudioGrapheme
import swing.Component

object AudioFileViewImpl {
  def apply[S <: Sys[S]](element: AudioGrapheme[S])(implicit tx: S#Tx): AudioFileView[S] = {
    val res = new Impl(tx.newHandle(element))
    guiFromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](holder: stm.Source[S#Tx, AudioGrapheme[S]])
    extends AudioFileView[S] {

    def component: Component = ???

    def guiInit() {


    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }
}