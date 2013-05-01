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
import java.awt.{Font, RenderingHints, BasicStroke, Color, Graphics2D}
import de.sciss.synth.proc.{Scan, Grapheme, Proc, ProcGroup, Sys}
import de.sciss.lucre.stm.Cursor
import de.sciss.lucre.stm
import de.sciss.synth.{SynthGraph, proc}
import de.sciss.synth.expr.{Longs, Spans}
import de.sciss.fingertree.RangedSeq
import javax.swing.UIManager
import java.util.Locale
import de.sciss.synth.proc.graph.scan
import de.sciss.synth
import de.sciss.lucre.bitemp.BiExpr

object TimelineViewImpl {
  private val colrDropRegionBg    = new Color(0xFF, 0xFF, 0xFF, 0x7F)
  private val strkDropRegion      = new BasicStroke(3f)
  private val colrRegionBg        = new Color(0x68, 0x68, 0x68)
  private val colrRegionBgSel     = Color.blue
  private final val hndlBaseline  = 12

  def apply[S <: Sys[S]](element: Element.ProcGroup[S])(implicit tx: S#Tx, cursor: Cursor[S]): TimelineView[S] = {
    val sampleRate  = 44100.0 // XXX TODO
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 600).toLong), sampleRate)
    val group       = element.entity
    import ProcGroup.serializer
    val groupH      = tx.newHandle[proc.ProcGroup[S]](group)
    //    group.nearestEventBefore(Long.MaxValue) match {
    //      case Some(stop) => Span(0L, stop)
    //      case _          => Span.from(0L)
    //    }

    val procMap   = tx.newInMemoryIDMap[TimelineProcView[S]]
    var rangedSeq = RangedSeq.empty[TimelineProcView[S], Long]

    group.iterator.foreach { case (span, seq) =>
      seq.foreach { timed =>
        // timed.span
        // val proc = timed.value
        val view = TimelineProcView(timed)
        procMap.put(timed.id, view)
        rangedSeq += view
      }
    }

    val res = new Impl[S](groupH, rangedSeq, tlm, cursor)
    guiFromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]](groupH: stm.Source[S#Tx, proc.ProcGroup[S]],
                                        procViews: RangedSeq[TimelineProcView[S], Long],
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
            val elem    = data.source()
            // val elemG = elem.entity
            val time    = drop.frame
            val spanV   = Span(time, time + data.drag.selection.length)
            val span    = Spans.newVar[S](Spans.newConst(spanV))
            val proc    = Proc[S]
            proc.name_=(elem.name)
            val scanw   = proc.scans.add("sig")
            // val scand   = proc.scans.add("dur")
            val grw     = Grapheme.Modifiable[S]
            // val grd     = Grapheme.Modifiable[S]
            val bi: Grapheme.TimedElem[S] = BiExpr(Longs.newVar(Longs.newConst(time)), data.source().entity)
            grw.add(bi)
            // val gv = Grapheme.Value.Curve
            // val crv = gv(dur -> stepShape)
            // grd.add(time -> crv)
            scanw.source_=(Some(Scan.Link.Grapheme(grw)))
            // scand.source_=(Some(Scan.Link.Grapheme(grd)))
            val sg = SynthGraph {
              import synth._
              import ugen._
              val sig   = scan("sig").ar(0)
              // val env   = EnvGen.ar(Env.linen(0.2, (duri - 0.4).max(0), 0.2))
              Out.ar(0, sig /* * env */)
            }
            proc.graph_=(sg)
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

        font = {
          val f = UIManager.getFont("Slider.font", Locale.US)
          if (f != null) f.deriveFont(math.min(f.getSize2D, 9.5f)) else new Font("SansSerif", Font.PLAIN, 9)
        }
        // setOpaque(true)

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

          val visi      = timelineModel.visible
          val clipOrig  = g.getClip

          procViews.filterOverlaps((visi.start, visi.stop)).foreach { pv =>
            val selected  = false
            val muted     = false

            def drawProc(x1: Int, x2: Int) {
              val py    = 0
              val px    = x1
              val pw    = x2 - x1
              val ph    = 64
              // if (stakeBorderViewMode != StakeBorderViewMode.None) {
              g.setColor(if (selected) colrRegionBgSel else colrRegionBg)
              g.fillRoundRect(px, py, pw, ph, 5, 5)
              // }



              // if (stakeBorderViewMode == StakeBorderViewMode.TitledBox) {
              g.clipRect(px + 2, py + 2, pw - 4, ph - 4)
              g.setColor(Color.white)
              // possible unicodes: 2327 23DB 24DC 25C7 2715 29BB
              g.drawString(if (muted) "\u23DB " + pv.name else pv.name, px + 4, py + hndlBaseline)
              //              stakeInfo(ar).foreach(info => {
              //                g2.setColor(Color.yellow)
              //                g2.drawString(info, x + 4, y + hndlBaseline + hndlExtent)
              //              })
              g.setClip(clipOrig)
              // }
            }

            pv.span match {
              case Span(start, stop) =>
                val x1 = frameToScreen(start).toInt
                val x2 = frameToScreen(stop ).toInt
                drawProc(x1, x2)

              case Span.From(start) =>
                val x1 = frameToScreen(start).toInt
                drawProc(x1, w + 5)

              case Span.Until(stop) =>
                val x2 = frameToScreen(stop).toInt
                drawProc(-5, x2)

              case Span.All =>
                drawProc(-5, w + 5)

              case _ => // don't draw Span.Void
            }
          }

          paintPosAndSelection(g, h)

          if (audioDnD.isDefined) audioDnD.foreach { drop =>
            g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
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