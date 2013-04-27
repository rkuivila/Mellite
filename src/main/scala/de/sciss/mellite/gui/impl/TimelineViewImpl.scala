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
import java.awt.{BasicStroke, Color, Graphics2D}
import de.sciss.synth.proc.{Proc, ProcGroup, Sys}
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.stm
import de.sciss.synth.proc
import de.sciss.synth.expr.Spans

object TimelineViewImpl {
  private val colrDropRegionBg  = new Color(0xFF, 0xFF, 0xFF, 0x7F)
  private val strkDropRegion    = new BasicStroke(3f)

  def apply[S <: Sys[S]](element: Element.ProcGroup[S])(implicit tx: S#Tx, cursor: Cursor[S]): TimelineView[S] = {
    val sampleRate  = 44100.0 // XXX TODO
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 600).toLong), sampleRate)
    val group       = element.entity
    import ProcGroup.serializer
    val groupH      = tx.newHandle[proc.ProcGroup[S]](group)
    val res         = new Impl[S](groupH, tlm, cursor)
    //    group.nearestEventBefore(Long.MaxValue) match {
    //      case Some(stop) => Span(0L, stop)
    //      case _          => Span.from(0L)
    //    }

    group.iterator.foreach { case (span, seq) =>
      seq.foreach { timed =>
        // timed.span
        val proc = timed.value

      }
    }

    guiFromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](groupH: stm.Source[S#Tx, proc.ProcGroup[S]],
                                        timelineModel: TimelineModel, cursor: Cursor[S]) extends TimelineView[S] {
    impl =>

    def guiInit() {
      component
    }

    private def dropAudioRegion(drop: AudioFileDnD.Drop, data: AudioFileDnD.Data[S]): Boolean = {
      cursor.step { implicit tx =>
        val group = groupH()
        group.modifiableOption match {
          case Some(groupM) =>
            val elem  = data.source()
            // val elemG = elem.entity
            val spanV = Span(drop.frame, drop.frame + data.drag.selection.length)
            val span  = Spans.newVar[S](Spans.newConst(spanV))
            val proc  = Proc[S]
            proc.name_=(elem.name)
            // proc.scans
            // proc.graphemes
            groupM.add(span, proc)
            true

          case _ => false
        }
      }
    }

    private final class View extends AbstractTimelineView {
      view =>
      // import AbstractTimelineView._
      protected def timelineModel = impl.timelineModel

      protected object mainView extends Component with AudioFileDnD[S] {
        protected def timelineModel = impl.timelineModel

        private var audioDnD = Option.empty[AudioFileDnD.Drop]

        protected def updateDnD(drop: Option[AudioFileDnD.Drop]) {
          audioDnD = drop
          repaint()
        }

        protected def acceptDnD(drop: AudioFileDnD.Drop, data: AudioFileDnD.Data[S]): Boolean =
          dropAudioRegion(drop, data)

        override protected def paintComponent(g: Graphics2D) {
          super.paintComponent(g)
          val w = peer.getWidth
          val h = peer.getHeight
          g.setColor(Color.darkGray) // g.setPaint(pntChecker)
          g.fillRect(0, 0, w, h)
          paintPosAndSelection(g, h)

          if (audioDnD.isDefined) audioDnD.foreach { drop =>
            val x1 = frameToScreen(drop.frame).toInt
            val x2 = frameToScreen(drop.frame + drop.drag.selection.length).toInt
            g.setColor(colrDropRegionBg)
            val strkOrig = g.getStroke
            g.setStroke(strkDropRegion)
            val y   = drop.y - drop.y % 32
            val x1b = math.min(x1 + 1, x2)
            val x2b = math.max(x1b, x2 - 1)
            g.drawRect(x1b, y + 1, x2b - x1b, 64)
            g.setStroke(strkOrig)
          }
        }
      }
    }

    private lazy val view = new View

    lazy val component: Component = view.component
  }
}