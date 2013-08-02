package de.sciss
package mellite
package gui

import de.sciss.synth.proc.Sys
import de.sciss.mellite.gui.impl.TimelineProcView
import de.sciss.span.Span
import de.sciss.audiowidgets.TimelineCanvas

trait TimelineProcCanvas[S <: Sys[S]] extends TimelineCanvas {
  def selectionModel: ProcSelectionModel[S]
  // def group(implicit tx: S#Tx): ProcGroup[S]
  def intersect(span: Span): Iterator[TimelineProcView[S]]

  def findRegion(frame: Long, hitTrack: Int): Option[TimelineProcView[S]]
  def screenToTrack(y: Int): Int
}