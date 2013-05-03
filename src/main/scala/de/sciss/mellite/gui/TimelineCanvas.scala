package de.sciss
package mellite
package gui

import scala.swing.Component
import de.sciss.synth.proc.Sys
import de.sciss.mellite.Element.ProcGroup
import de.sciss.mellite.gui.impl.TimelineProcView
import de.sciss.span.Span

trait TimelineCanvas {
  def timelineModel: TimelineModel
  def canvasComponent: Component

  def frameToScreen(frame: Long): Double
  def screenToFrame(screen: Int): Double
  def clipVisible(frame: Double): Long
}

trait TimelineProcCanvas[S <: Sys[S]] extends TimelineCanvas {
  def selectionModel: ProcSelectionModel[S]
  // def group(implicit tx: S#Tx): ProcGroup[S]
  def intersect(span: Span): Iterator[TimelineProcView[S]]
}