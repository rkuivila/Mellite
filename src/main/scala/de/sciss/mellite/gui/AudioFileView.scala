package de.sciss.mellite
package gui

import de.sciss.synth.proc.{ArtifactStore, Sys}
import swing.Component
import Element.AudioGrapheme
import impl.{AudioFileViewImpl => Impl}

object AudioFileView {
  def apply[S <: Sys[S]](element: AudioGrapheme[S])(implicit tx: S#Tx, store: ArtifactStore[S]): AudioFileView[S] =
    Impl(element)
}
trait AudioFileView[S <: Sys[S]] {
  def component: Component
  def element(implicit tx: S#Tx): AudioGrapheme[S]
}