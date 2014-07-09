/*
 *  TimelineProcCanvas.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite
package gui

import de.sciss.span.Span
import de.sciss.audiowidgets.TimelineCanvas
import de.sciss.mellite.gui.impl.timeline.ProcView
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Timeline

trait TimelineProcCanvas[S <: Sys[S]] extends TimelineCanvas {
  def timeline(implicit tx: S#Tx): Timeline[S]

  def selectionModel: ProcSelectionModel[S]

  def intersect(span: Span): Iterator[ProcView[S]]

  def findRegion(frame: Long, hitTrack: Int): Option[ProcView[S]]
  def screenToTrack(y: Int): Int
}