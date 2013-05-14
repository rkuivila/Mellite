package de.sciss.mellite
package gui

import de.sciss.synth.proc.Sys
import swing.Component
import Element.AudioGrapheme
import impl.{AudioFileViewImpl => Impl}
import de.sciss.lucre.stm.Disposable

object AudioFileView {
  def apply[S <: Sys[S]](document: Document[S], element: AudioGrapheme[S])(implicit tx: S#Tx): AudioFileView[S] =
    Impl(document, element)
}
trait AudioFileView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component
  def element(implicit tx: S#Tx): AudioGrapheme[S]
}