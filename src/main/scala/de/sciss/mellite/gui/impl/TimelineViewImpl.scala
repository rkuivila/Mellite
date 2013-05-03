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

package de.sciss
package mellite
package gui
package impl

import scala.swing.{Action, BorderPanel, Orientation, BoxPanel, Component}
import span.{Span, SpanLike}
import mellite.impl.TimelineModelImpl
import java.awt.{Font, RenderingHints, BasicStroke, Color, Graphics2D}
import de.sciss.synth.proc.{Attribute, ProcGroup, ProcTransport, Sys, TimedProc}
import lucre.{bitemp, stm}
import lucre.stm.{IdentifierMap, Cursor}
import synth.proc
import fingertree.RangedSeq
import javax.swing.{KeyStroke, UIManager}
import java.util.Locale
import bitemp.BiGroup
import audiowidgets.Transport
import scala.swing.Swing._
import java.awt.event.{KeyEvent, ActionEvent, ActionListener}
import de.sciss.desktop.FocusType
import de.sciss.synth.expr.{ExprImplicits, SpanLikes, Ints}
import de.sciss.lucre.expr.Expr

object TimelineViewImpl {
  private val colrDropRegionBg    = new Color(0xFF, 0xFF, 0xFF, 0x7F)
  private val strkDropRegion      = new BasicStroke(3f)
  private val colrRegionBg        = new Color(0x68, 0x68, 0x68)
  private val colrRegionBgSel     = Color.blue
  private final val hndlExtent    = 15
  private final val hndlBaseline  = 12

  private val NoMove  = TrackTool.Move(deltaTime = 0L, deltaTrack = 0, copy = false)
  private val MinDur  = 32

  def apply[S <: Sys[S]](document: Document[S], element: Element.ProcGroup[S])
                        (implicit tx: S#Tx): TimelineView[S] = {
    val sampleRate  = 44100.0 // XXX TODO
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 600).toLong), sampleRate)
    val group       = element.entity
    import ProcGroup.serializer
    val groupH      = tx.newHandle[proc.ProcGroup[S]](group)
    //    group.nearestEventBefore(Long.MaxValue) match {
    //      case Some(stop) => Span(0L, stop)
    //      case _          => Span.from(0L)
    //    }

    import document.{cursor, inMemory}
    val procMap     = tx.newInMemoryIDMap[TimelineProcView[S]]
    val transp      = proc.Transport[S, document.I](group, sampleRate = sampleRate)
    val aural   = proc.AuralPresentation.run[S, document.I](transp, document.aural)
    // XXX TODO dispose transp and aural

    val view    = new Impl[S](groupH, transp, procMap, tlm, document.cursor)

    transp.reactTx { implicit tx => {
      case proc.Transport.Play(t, time) => view.startedPlaying(time)
      case proc.Transport.Stop(t, time) => view.stoppedPlaying(time)
      case _ => // proc.Transport.Advance(t, time, isSeek, isPlaying, _, _, _) =>
    }}

    group.iterator.foreach { case (span, seq) =>
      seq.foreach { timed =>
        view.addProc(span, timed)
      }
    }

    val obs = group.changed.reactTx { implicit tx => (upd: ProcGroup.Update[S]) => {
      // val _group = upd.group
      upd.changes.foreach {
        case BiGroup.Added  (span, timed) =>
          // println(s"Added   $span, $timed")
          view.addProc(span, timed)
          guiFromTx(view.component.repaint())  // XXX TODO: optimize dirty rectangle

        case BiGroup.Removed(span, timed) => println(s"Removed $span, $timed")
        case BiGroup.ElementMoved  (timed, spanCh ) => println(s"Moved   $timed, $spanCh")
        case BiGroup.ElementMutated(timed, procUpd) => println(s"Mutated $timed, $procUpd")
      }
    }}
    // XXX TODO: dispose observer eventually

    guiFromTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](groupH: stm.Source[S#Tx, ProcGroup[S]],
                                        transp: ProcTransport[S],
                                        procMap: IdentifierMap[S#ID, S#Tx, TimelineProcView[S]],
                                        timelineModel: TimelineModel, cursor: Cursor[S]) extends TimelineView[S] {
    impl =>

    import cursor.step

    private var procViews = RangedSeq.empty[TimelineProcView[S], Long]

    private var timerFrame  = 0L
    private var timerSys    = 0L
    private val srm         = 0.001 * transp.sampleRate
    private val timer       = new javax.swing.Timer(31, new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val elapsed             = ((System.currentTimeMillis() - timerSys) * srm).toLong
        timelineModel.position  = timerFrame + elapsed
      }
    })

    private var transportStrip: Component with Transport.ButtonStrip = _
    private val selectionModel = ProcSelectionModel[S]

    var component: Component = _

    def startedPlaying(time: Long)(implicit tx: S#Tx) {
      guiFromTx {
        timer.stop()
        timerFrame  = time
        timerSys    = System.currentTimeMillis()
        timer.start()
        transportStrip.button(Transport.Play).foreach(_.selected = true )
        transportStrip.button(Transport.Stop).foreach(_.selected = false)
      }
    }

    def stoppedPlaying(time: Long)(implicit tx: S#Tx) {
      guiFromTx {
        timer.stop()
        timelineModel.position = time
        transportStrip.button(Transport.Play).foreach(_.selected = false)
        transportStrip.button(Transport.Stop).foreach(_.selected = true )
      }
    }

    private def rtz() {
      stop()
      val start = timelineModel.bounds.start
      timelineModel.position  = start
      timelineModel.visible   = Span(start, start + timelineModel.visible.length)
    }

    private def rewind() {

    }

    private def playOrStop() {
      step { implicit tx =>
        if (transp.isPlaying) transp.stop() else {
          transp.seek(timelineModel.position)
          transp.play()
        }
      }
    }

    private def stop() {
      step { implicit tx => transp.stop() }
    }

    private def play() {
      step { implicit tx =>
        transp.stop()
        transp.seek(timelineModel.position)
        transp.play()
      }
    }

    private def ffwd() {

    }

    def guiInit() {
      val timeDisp    = TimeDisplay(timelineModel)
      val view        = new View

      import Transport.{Action => _, _}
      transportStrip = Transport.makeButtonStrip(Seq(
        GoToBegin   { rtz()    },
        Rewind      { rewind() },
        Stop        { stop()   },
        Play        { play()   },
        FastForward { ffwd()   },
        Loop        {}
      ))
      transportStrip.button(Stop).foreach(_.selected = true)

      val toolCursor  = TrackTool.cursor[S](view)
      val toolMove    = TrackTool.move  [S](view)

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          TrackTools.palette(view.trackTools, Vector(toolCursor, toolMove)),
          HGlue,
          HStrut(4),
          timeDisp.component,
          HStrut(8),
          transportStrip,
          HStrut(4)
        )
      }
      import desktop.Implicits._
      transportPane.addAction("playstop", focus = FocusType.Window, action = new Action("playstop") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, 0))
        def apply() {
          playOrStop()
        }
      })
      transportPane.addAction("rtz", focus = FocusType.Window, action = new Action("rtz") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0))
        def apply() {
          transportStrip.button(GoToBegin).foreach(_.doClick())
        }
      })

      val pane  = new BorderPanel {
        layoutManager.setVgap(2)
        add(transportPane,  BorderPanel.Position.North )
        add(view.component, BorderPanel.Position.Center)
      }

      component = pane
    }

    def addProc(span: SpanLike, timed: TimedProc[S])(implicit tx: S#Tx) {
      // timed.span
      // val proc = timed.value
      val view = TimelineProcView(timed)
      procMap.put(timed.id, view)
      procViews += view
    }

    private def dropAudioRegion(drop: AudioFileDnD.Drop, data: AudioFileDnD.Data[S]): Boolean = step { implicit tx =>
      val group = groupH()
      group.modifiableOption match {
        case Some(groupM) =>
          DropAudioRegionAction(groupM, drop, data)
          true
        case _ => false
      }
    }

    private final class View extends TimelineProcCanvasImpl[S] {
      view =>
      // import AbstractTimelineView._
      def timelineModel   = impl.timelineModel
      def selectionModel  = impl.selectionModel

      def intersect(span: Span): Iterator[TimelineProcView[S]] = procViews.filterOverlaps((span.start, span.stop))

      protected def commitToolChanges(value: Any) {
        step { implicit tx =>
          selectionModel.iterator.foreach { pv =>
            val span  = pv.spanSource()
            val proc  = pv.procSource()
            value match {
              case TrackTool.Move(deltaTime, deltaTrack, copy) =>
                if (deltaTrack != 0) {
                   // XXX TODO: could check for Expr.Const here and Expr.Var.
                  // in the case of const, just overwrite, in the case of
                  // var, check the value stored in the var, and update the var
                  // instead (recursion). otherwise, it will be some combinatory
                  // expression, and we could decide to construct a binary op instead!
                  val attr      = proc.attributes
                  val trackOld  = attr.get(ProcKeys.track) match {
                    case Some(i: Attribute.Int[S]) => i.peer.value
                    case _ => 0
                  }
                  val trackNew = math.max(0, trackOld + deltaTrack)
                  attr.put(ProcKeys.track, Attribute.Int(Ints.newConst(trackNew)))
                }

                val oldSpan   = span.value
                val minStart  = timelineModel.bounds.start
                val deltaC    = if (deltaTime >= 0) deltaTime else oldSpan match {
                  case Span.HasStart(oldStart) => math.max(-(oldStart - minStart), deltaTime)
                  case Span.HasStop (oldStop)  => math.max(-(oldStop  - minStart + MinDur), deltaTime)
                }
                if (deltaC != 0L) {
                  val imp = ExprImplicits[S]
                  import imp._
                  span match {
                    // case Expr.Const(vl) =>
                    case Expr.Var(vr) =>
                      vr.transform {
                        case Expr.Const(vl) => vl.shift(deltaC) // fold constant
                        // case SpanLikes.BinaryOp.Shift(a, b) => ...
                        case ex             => ex.shift(deltaC) // compose binary op. XXX TODO: should fold bin ops
                      }
                    case _ => // span.shift(deltaC)
                  }
                }
              case _ =>
            }
          }
        }
      }

      object canvasComponent extends Component with AudioFileDnD[S] with sonogram.PaintController {
        protected def timelineModel = impl.timelineModel

        private var audioDnD = Option.empty[AudioFileDnD.Drop]

        // var visualBoost = 1f
        private var sonoBoost = 1f
        // private var regionViewMode: RegionViewMode = RegionViewMode.TitledBox

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

        def imageObserver = peer

        def adjustGain(amp: Float, pos: Double) = amp * sonoBoost

        override protected def paintComponent(g: Graphics2D) {
          super.paintComponent(g)
          val w = peer.getWidth
          val h = peer.getHeight
          g.setColor(Color.darkGray) // g.setPaint(pntChecker)
          g.fillRect(0, 0, w, h)

          val total     = timelineModel.bounds
          val visi      = timelineModel.visible
          val clipOrig  = g.getClip
          val cr        = clipOrig.getBounds

          val regionViewMode  = trackTools.regionViewMode
          val visualBoost     = trackTools.visualBoost

          val hndl = regionViewMode match {
             case RegionViewMode.None       => 0
             case RegionViewMode.Box        => 1
             case RegionViewMode.TitledBox  => hndlExtent
          }

          val sel       = selectionModel
          val tState    = toolState
          val moveState = tState match {
            case Some(mv: TrackTool.Move) => mv
            case _ => NoMove
          }

          procViews.filterOverlaps((visi.start, visi.stop)).foreach { pv =>
            val selected  = sel.contains(pv)
            val muted     = false

            def drawProc(start: Long, x1: Int, x2: Int) {
              val py    = (if (selected) math.max(0, pv.track + moveState.deltaTrack) else pv.track) * 32
              val px    = x1
              val pw    = x2 - x1
              val ph    = 64

              val px1C    = math.max(px + 1, cr.x - 2)
              val px2C    = math.min(px + pw, cr.x + cr.width + 3)
              if (px1C < px2C) {  // skip this if we are not overlapping with clip

                if (regionViewMode != RegionViewMode.None) {
                  g.setColor(if (selected) colrRegionBgSel else colrRegionBg)
                  g.fillRoundRect(px, py, pw, ph, 5, 5)
                }

                pv.audio.foreach { segm =>
                  val innerH  = ph - (hndl + 1)

                  val sono = pv.sono.getOrElse {
                    val res = SonogramManager.acquire(segm.value.artifact)
                    pv.sono = Some(res)
                    res
                  }
                  val audio   = segm.value
                  val dStart  = audio.offset /* - start */ + (/* start */ - segm.span.start)
                  val startC  = math.max(0.0, screenToFrame(px1C))
                  val stopC   = screenToFrame(px2C)
                  sonoBoost   = audio.gain.toFloat * visualBoost
                  sono.paint(startC + dStart, stopC + dStart, g, px1C, py + hndl, px2C - px1C, innerH, this)
                }

                if (regionViewMode == RegionViewMode.TitledBox) {
                  val name = pv.name // .orElse(pv.audio.map(_.value.artifact.nameWithoutExtension))
                  g.clipRect(px + 2, py + 2, pw - 4, ph - 4)
                  g.setColor(Color.white)
                  // possible unicodes: 2327 23DB 24DC 25C7 2715 29BB
                  g.drawString(if (muted) "\u23DB " + name else name, px + 4, py + hndlBaseline)
                  //              stakeInfo(ar).foreach { info =>
                  //                g2.setColor(Color.yellow)
                  //                g2.drawString(info, x + 4, y + hndlBaseline + hndlExtent)
                  //              }
                  g.setClip(clipOrig)
                }
              }
            }

            def deltaTimeWithStart(start: Long): Long =
              if (selected) {
                val dt0 = moveState.deltaTime
                if (dt0 >= 0) dt0 else {
                  math.max(-(start - total.start), dt0)
                }
              } else 0L

            pv.span match {
              case Span(start, stop) =>
                val dt = deltaTimeWithStart(start)
                val x1 = frameToScreen(start + dt).toInt
                val x2 = frameToScreen(stop  + dt).toInt
                drawProc(start, x1, x2)

              case Span.From(start) =>
                val dt = deltaTimeWithStart(start)
                val x1 = frameToScreen(start + dt).toInt
                drawProc(start, x1, w + 5)

              case Span.Until(stop) =>
                val dt = if (selected) {
                  val dt0 = moveState.deltaTime
                  if (dt0 >= 0) dt0 else {
                    math.max(-(stop - total.start + MinDur), dt0)
                  }
                } else 0L
                val x2 = frameToScreen(stop + dt).toInt
                drawProc(Long.MinValue, -5, x2)

              case Span.All =>
                drawProc(Long.MinValue, -5, w + 5)

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
  }
}