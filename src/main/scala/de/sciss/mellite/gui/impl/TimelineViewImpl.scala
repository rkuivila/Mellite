/*
 *  TimelineViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import scala.swing.Component
import de.sciss.span.Span
import de.sciss.mellite.impl.TimelineModelImpl
import java.awt.{Color, Graphics2D}
import de.sciss.synth.proc.Sys

object TimelineViewImpl {
  def apply[S <: Sys[S]](element: Element.ProcGroup[S])(implicit tx: S#Tx): TimelineView[S] = {
    val sampleRate  = 44100.0 // XXX TODO
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 600).toLong), sampleRate)
    val res         = new Impl[S](tlm)
    val group       = element.entity
    //    group.nearestEventBefore(Long.MaxValue) match {
    //      case Some(stop) => Span(0L, stop)
    //      case _          => Span.from(0L)
    //    }

    guiFromTx(res.guiInit())
    res
  }

  private final class View[S <: Sys[S]](protected val timelineModel: TimelineModel) extends AbstractTimelineView {
    view =>
    // import AbstractTimelineView._

    protected object mainView extends Component with AudioFileDnD[S] {
      protected def timelineModel = view.timelineModel

      protected def updateDnD(drop: Option[AudioFileDnD.Drop]) {
        println(s"Drop = $drop")
      }

      protected def acceptDnD(data: AudioFileDnD.Data[S]): Boolean = {
        println("Accept")
        true
      }

      override protected def paintComponent(g: Graphics2D) {
        super.paintComponent(g)
        val w = peer.getWidth
        val h = peer.getHeight
        g.setColor(Color.darkGray) // g.setPaint(pntChecker)
        g.fillRect(0, 0, w, h)
        paintPosAndSelection(g, h)
      }
    }
  }

  private final class Impl[S <: Sys[S]](timelineModel: TimelineModel) extends TimelineView[S] {
    def guiInit() {
      component
    }

    private lazy val view = new View[S](timelineModel)

    lazy val component: Component = view.component
  }
}