package de.sciss
package mellite
package gui

import desktop.Window
import synth.proc.Sys
import impl.{TimelineFrameImpl => Impl}

object TimelineFrame {
  def apply[S <: Sys[S]](document: Document[S], name: String, group: Element.ProcGroup[S])
                        (implicit tx: S#Tx): TimelineFrame[S] =
    Impl(document, name, group)
}
trait TimelineFrame[S <: Sys[S]] {
  def window: Window
  def view: TimelineView[S]
}