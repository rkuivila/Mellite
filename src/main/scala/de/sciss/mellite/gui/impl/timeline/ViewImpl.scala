/*
 *  ViewImpl.scala
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
package timeline

import scala.swing.{Swing, Slider, Action, BorderPanel, Orientation, BoxPanel, Component}
import de.sciss.span.{Span, SpanLike}
import de.sciss.mellite.impl.InsertAudioRegion
import java.awt.{Rectangle, TexturePaint, Font, RenderingHints, BasicStroke, Color, Graphics2D}
import de.sciss.synth
import de.sciss.desktop
import de.sciss.lucre.stm
import de.sciss.sonogram
import de.sciss.lucre.stm.{Disposable, IdentifierMap, Cursor}
import de.sciss.synth.{Curve, proc}
import de.sciss.fingertree.RangedSeq
import javax.swing.{KeyStroke, UIManager}
import java.util.Locale
import de.sciss.lucre.bitemp.{BiExpr, BiGroup}
import de.sciss.audiowidgets.{TimelineModel, Transport}
import scala.swing.Swing._
import java.awt.event.{KeyEvent, ActionEvent}
import de.sciss.desktop.FocusType
import Predef.{any2stringadd => _, _}
import de.sciss.lucre.event.Change
import scala.concurrent.stm.Ref
import de.sciss.lucre.expr.Expr
import de.sciss.synth.expr.{Doubles, ExprImplicits, Longs, SpanLikes}
import java.awt.geom.Path2D
import java.awt.image.BufferedImage
import scala.swing.event.ValueChanged
import de.sciss.synth.proc.{FadeSpec, AuralPresentation, Attribute, Grapheme, ProcKeys, Proc, Scan, Sys, AuralSystem, ProcGroup, ProcTransport, TimedProc}
import de.sciss.audiowidgets.impl.TimelineModelImpl

object ViewImpl {
  private val colrDropRegionBg    = new Color(0xFF, 0xFF, 0xFF, 0x7F)
  private val strkDropRegion      = new BasicStroke(3f)
  private val colrRegionBg        = new Color(0x68, 0x68, 0x68)
  private val colrRegionBgSel     = Color.blue
  private val colrBgMuted         = new Color(0xFF, 0xFF, 0xFF, 0x60)
  private final val hndlExtent    = 15
  private final val hndlBaseline  = 12
  private val colrFade = new Color(0x05, 0xAF, 0x3A)
  private val pntFade = {
    val img = new BufferedImage(4, 2, BufferedImage.TYPE_INT_ARGB)
    img.setRGB(0, 0, 4, 2, Array(
      0xFF05AF3A, 0x00000000, 0x00000000, 0x00000000,
      0x00000000, 0x00000000, 0xFF05AF3A, 0x00000000
    ), 0, 4)
    new TexturePaint(img, new Rectangle(0, 0, 4, 2))
  }

  private val NoMove      = TrackTool.Move(deltaTime = 0L, deltaTrack = 0, copy = false)
  private val NoResize    = TrackTool.Resize(deltaStart = 0L, deltaStop = 0L)
  private val NoGain      = TrackTool.Gain(1f)
  private val NoFade      = TrackTool.Fade(0L, 0L, 0f, 0f)
  private val NoFunction  = TrackTool.Function(-1, Span(0L, 0L))
  private val MinDur      = 32

  private val logEnabled  = false
  private val DEBUG       = false

  private def log(what: => String): Unit =
    if (logEnabled) println(s"<timeline> $what")

  def apply[S <: Sys[S]](document: Document[S], element: Element.ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], aural: AuralSystem): TimelineView[S] = {
    val sampleRate  = 44100.0 // XXX TODO
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 60 * 60).toLong), sampleRate)
    tlm.visible     = Span(0L, (sampleRate * 60 * 2).toLong)
    val group       = element.entity
    import ProcGroup.serializer
    val groupH      = tx.newHandle[proc.ProcGroup[S]](group)
    //    group.nearestEventBefore(Long.MaxValue) match {
    //      case Some(stop) => Span(0L, stop)
    //      case _          => Span.from(0L)
    //    }

    var disp = List.empty[Disposable[S#Tx]]

    // XXX TODO --- should use TransportView now!

    import document.inMemoryBridge // {cursor, inMemory}
    val procMap   = tx.newInMemoryIDMap[ProcView[S]]
    disp ::= procMap
    val transp    = proc.Transport[S, document.I](group, sampleRate = sampleRate)
    disp ::= transp
    val auralView = proc.AuralPresentation.runTx[S](transp, aural)
    disp ::= auralView

    val view    = new Impl(document, groupH, transp, procMap, tlm, auralView)

    val obsTransp = transp.react { implicit tx => {
      case proc.Transport.Play(t, time) => view.startedPlaying(time)
      case proc.Transport.Stop(t, time) => view.stoppedPlaying(time)
      case _ => // proc.Transport.Advance(t, time, isSeek, isPlaying, _, _, _) =>
    }}
    disp ::= obsTransp

    group.iterator.foreach { case (span, seq) =>
      seq.foreach { timed =>
        view.addProc(span, timed, repaint = false)
      }
    }

    def muteChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attributes
      val muted   = attr[Attribute.Boolean](ProcKeys.attrMute).exists(_.value)
      view.procMuteChanged(timed, muted)
    }

    def fadeChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attributes
      val fadeIn  = attr[Attribute.FadeSpec](ProcKeys.attrFadeIn ).map(_.value).getOrElse(TrackTool.EmptyFade)
      val fadeOut = attr[Attribute.FadeSpec](ProcKeys.attrFadeOut).map(_.value).getOrElse(TrackTool.EmptyFade)
      view.procFadeChanged(timed, fadeIn, fadeOut)
    }

    def attrChanged(timed: TimedProc[S], name: String)(implicit tx: S#Tx): Unit =
      name match {
        case ProcKeys.attrMute => muteChanged(timed)
        case ProcKeys.attrFadeIn | ProcKeys.attrFadeOut => fadeChanged(timed)
        case _ =>
      }

    val obsGroup = group.changed.react { implicit tx => _.changes.foreach {
      case BiGroup.Added  (span, timed) =>
        // println(s"Added   $span, $timed")
        view.addProc(span, timed, repaint = true)

      case BiGroup.Removed(span, timed) =>
        // println(s"Removed $span, $timed")
        view.removeProc(span, timed)

      case BiGroup.ElementMoved  (timed, spanChange) =>
        // println(s"Moved   $timed, $spanCh")
        view.procMoved(timed, spanCh = spanChange, trackCh = Change(0, 0))

      case BiGroup.ElementMutated(timed, procUpd) =>
        if (DEBUG) println(s"Mutated $timed, $procUpd")
        procUpd.changes.foreach {
          case Proc.AssociationAdded  (key) =>
            key match {
              case Proc.AttributeKey(name) => attrChanged(timed, name)
              case Proc.ScanKey(name) =>
            }
          case Proc.AssociationRemoved(key) =>
            key match {
              case Proc.AttributeKey(name) => attrChanged(timed, name)
              case Proc.ScanKey     (name) =>
            }
          case Proc.AttributeChange(name, attr, ach) =>
            (name, ach) match {
              case (ProcKeys.attrTrack, Change(before: Int, now: Int)) =>
                view.procMoved(timed, spanCh = Change(Span.Void, Span.Void), trackCh = Change(before, now))

              case _ => attrChanged(timed, name)
            }

          case Proc.ScanChange(name, scan, scanUpds) =>
            scanUpds.foreach {
              case Scan.GraphemeChange(grapheme, segms) =>
                if (name == ProcKeys.graphAudio) {
                  timed.span.value match {
                    case Span.HasStart(startFrame) =>
                      val segmOpt = segms.find(_.span.contains(startFrame)) match {
                        case Some(segm: Grapheme.Segment.Audio) => Some(segm)
                        case _ => None
                      }
                      view.procAudioChanged(timed, segmOpt)
                    case _ =>
                  }
                }

              case _ => // Scan.SinkAdded(_) | Scan.SinkRemoved(_) | Scan.SourceAdded(_) | Scan.SourceRemoved(_)
            }

          case Proc.GraphChange(ch) =>
        }
    }}
    disp ::= obsGroup

    view.disposables.set(disp)(tx.peer)

    guiFromTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](document: Document[S], groupH: stm.Source[S#Tx, ProcGroup[S]],
                                        transp: ProcTransport[S],
                                        procMap: IdentifierMap[S#ID, S#Tx, ProcView[S]],
                                        val timelineModel: TimelineModel,
                                        auralView: AuralPresentation[S])
                                       (implicit cursor: Cursor[S])
    extends TimelineView[S] with ComponentHolder[Component] {
    impl =>

    import cursor.step

    private var procViews = RangedSeq.empty[ProcView[S], Long]

    private var timerFrame  = 0L
    private var timerSys    = 0L
    private val srm         = 0.001 * transp.sampleRate
    private val timer       = new javax.swing.Timer(31,
      Swing.ActionListener(timelineModel.modifiableOption.fold((_: ActionEvent) => ()) { mod => (e: ActionEvent) =>
        val elapsed   = ((System.currentTimeMillis() - timerSys) * srm).toLong
        mod.position  = timerFrame + elapsed
      })
    )

    private var transportStrip: Component with Transport.ButtonStrip = _
    private val selectionModel = ProcSelectionModel[S]

    private var view: View    = _
    val disposables           = Ref(List.empty[Disposable[S#Tx]])

    private lazy val toolCursor   = TrackTool.cursor  [S](view)
    private lazy val toolMove     = TrackTool.move    [S](view)
    private lazy val toolResize   = TrackTool.resize  [S](view)
    private lazy val toolGain     = TrackTool.gain    [S](view)
    private lazy val toolMute     = TrackTool.mute    [S](view)
    private lazy val toolFade     = TrackTool.fade    [S](view)
    private lazy val toolFunction = TrackTool.function[S](view)

    def dispose()(implicit tx: S#Tx): Unit = {
      timer.stop()  // save to call multiple times
      disposables.swap(Nil)(tx.peer).foreach(_.dispose())
      guiFromTx {
        val pit     = procViews.iterator
        procViews   = RangedSeq.empty
        pit.foreach { pv =>
          pv.release()
        }
      }
    }

    // ---- actions ----

    object stopAllSoundAction extends Action("StopAllSound") {
      def apply(): Unit =
        cursor.step { implicit tx =>
          auralView.stopAll
        }
    }

    object bounceAction extends Action("Bounce") {
      private var settings = ActionBounceTimeline.QuerySettings[S]()

      def apply(): Unit = {
        import ActionBounceTimeline._
        val window  = GUI.findWindow(component)
        val setUpd  = settings.copy(span = timelineModel.selection)
        val (_settings, ok) = query(setUpd, document, timelineModel, window = window)
        settings = _settings
        _settings.file match {
          case Some(file) if ok =>
            performGUI(document, _settings, groupH, file, window = window)
          case _ =>
        }
      }
    }

    object deleteAction extends Action("Delete") {
      def apply(): Unit =
        withSelection(implicit tx => deleteObjects)
    }

    object splitObjectsAction extends Action("Split Selected Objects") {
      def apply(): Unit = {
        val pos     = timelineModel.position
        val pos1    = pos - MinDur
        val pos2    = pos + MinDur
        withFilteredSelection(pv => pv.span.contains(pos1) && pv.span.contains(pos2)) { implicit tx =>
          splitObjects(pos)
        }
      }
    }

    private def withSelection(fun: S#Tx => TraversableOnce[ProcView[S]] => Unit): Unit = {
      val sel = selectionModel.iterator
      if (sel.hasNext) step { implicit tx => fun(tx)(sel) }
    }

    private def withFilteredSelection(p: ProcView[S] => Boolean)
                                     (fun: S#Tx => TraversableOnce[ProcView[S]] => Unit): Unit = {
      val sel = selectionModel.iterator
      val flt = sel.filter(p)
      if (flt.hasNext) step { implicit tx => fun(tx)(flt) }
    }

    def deleteObjects(views: TraversableOnce[ProcView[S]])(implicit tx: S#Tx): Unit =
      for (group <- groupH().modifiableOption; pv <- views) {
        val span  = pv.spanSource()
        val proc  = pv.procSource()
        group.remove(span, proc)
      }

    def splitObjects(time: Long)(views: TraversableOnce[ProcView[S]])(implicit tx: S#Tx): Unit =
      for {
        group             <- groupH().modifiableOption
        pv                <- views
      } {
        pv.spanSource() match {
          case Expr.Var(oldSpan) =>
            val imp = ExprImplicits[S]
            import imp._
            val leftProc  = pv.procSource()
            val rightProc = copyProc(oldSpan, leftProc)
            val rightSpan = oldSpan.value match {
              case Span.HasStart(leftStart) =>
                val _rightSpan = SpanLikes.newVar(oldSpan())
                ProcActions.resize(_rightSpan, rightProc, ProcActions.Resize(time - leftStart, 0L), timelineModel)
                _rightSpan

              case Span.HasStop(rightStop) =>
                SpanLikes.newVar(Span(time, rightStop))
            }

            oldSpan.value match {
              case Span.HasStop(rightStop) =>
                ProcActions.resize(oldSpan, leftProc, ProcActions.Resize(0L, time - rightStop), timelineModel)

              case Span.HasStart(leftStart) =>
                val leftSpan = Span(leftStart, time)
                oldSpan() = leftSpan
            }

            group.add(rightSpan, rightProc)

          case _ =>
        }
      }

    def copyProc(parentSpan: Expr[S, SpanLike], parent: Proc[S])(implicit tx: S#Tx): Proc[S] = {
      val res   = Proc[S]
      res.graph() = parent.graph
      parent.attributes.iterator.foreach { case (key, attr) =>
        val attrOut = attr.mkCopy()
        res.attributes.put(key, attrOut)
      }
      ProcActions.getAudioRegion(parentSpan, parent).foreach { case (time, audio) =>
        val imp = ExprImplicits[S]
        import imp._
        val scanw       = res.scans.add(ProcKeys.graphAudio)
        val grw         = Grapheme.Modifiable[S]
        val gStart      = Longs.newVar(time.value)
        val audioOffset = Longs.newVar(audio.offset.value)  // XXX TODO
        val audioGain   = Doubles.newVar(audio.gain.value)
        val gElem       = Grapheme.Elem.Audio(audio.artifact, audio.value.spec, audioOffset, audioGain)
        val bi: Grapheme.TimedElem[S] = BiExpr(gStart, gElem)
        grw.add(bi)
        scanw addSource grw
      }
      res
    }

    // ---- transport ----

    def startedPlaying(time: Long)(implicit tx: S#Tx): Unit =
      guiFromTx {
        timer.stop()
        timerFrame  = time
        timerSys    = System.currentTimeMillis()
        timer.start()
        transportStrip.button(Transport.Play).foreach(_.selected = true )
        transportStrip.button(Transport.Stop).foreach(_.selected = false)
      }

    def stoppedPlaying(time: Long)(implicit tx: S#Tx): Unit =
      guiFromTx {
        timer.stop()
        timelineModel.modifiableOption.foreach(_.position = time) // XXX TODO if Cursor follows Playhead
        transportStrip.button(Transport.Play).foreach(_.selected = false)
        transportStrip.button(Transport.Stop).foreach(_.selected = true )
      }

    private def rtz(): Unit = {
      stop()
      timelineModel.modifiableOption.foreach { mod =>
        val start = mod.bounds.start
        mod.position  = start
        mod.visible   = Span(start, start + mod.visible.length)
      }
    }

    private def rewind() = ()

    private def playOrStop(): Unit =
      step { implicit tx =>
        if (transp.isPlaying) transp.stop() else {
          transp.seek(timelineModel.position)
          transp.play()
        }
      }

    private def stop(): Unit =
      step { implicit tx => transp.stop() }

    private def play(): Unit =
      step { implicit tx =>
        transp.stop()
        transp.seek(timelineModel.position)
        transp.play()
      }

    private def ffwd() = ()

    def guiInit(): Unit = {
      import desktop.Implicits._

      val timeDisp    = TimeDisplay(timelineModel)
      view            = new View

      val ggVisualBoost = new Slider {
        min       = 0
        max       = 64
        value     = 0
        focusable = false
        peer.putClientProperty( "JComponent.sizeVariant", "small" )
        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            import synth._
            view.trackTools.visualBoost = value.linexp(0, 64, 1, 512) // .toFloat
        }
      }
      GUI.fixWidth(ggVisualBoost)

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

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          TrackTools.palette(view.trackTools, Vector(
            toolCursor, toolMove, toolResize, toolGain, toolFade /* , toolSlide*/ ,
            toolMute /* , toolAudition */, toolFunction)),
          HStrut(4),
          ggVisualBoost,
          HGlue,
          HStrut(4),
          timeDisp.component,
          HStrut(8),
          transportStrip,
          HStrut(4)
        )
      }
      transportPane.addAction("playstop", focus = FocusType.Window, action = new Action("playstop") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_SPACE, 0))
        def apply(): Unit = playOrStop()
      })
      transportPane.addAction("rtz", focus = FocusType.Window, action = new Action("rtz") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0))
        def apply(): Unit =
          transportStrip.button(GoToBegin).foreach(_.doClick())
      })

      val pane = new BorderPanel {
        layoutManager.setVgap(2)
        add(transportPane , BorderPanel.Position.North )
        add(view.component, BorderPanel.Position.Center)
      }

      comp = pane
    }

    def addProc(span: SpanLike, timed: TimedProc[S], repaint: Boolean)(implicit tx: S#Tx): Unit = {
      log(s"addProc($span, $timed)")
      // timed.span
      // val proc = timed.value
      val pv = ProcView(timed)
      procMap.put(timed.id, pv)
      if (repaint) {
        procViews += pv
        guiFromTx(view.canvasComponent.repaint()) // XXX TODO: optimize dirty rectangle
      }  else {
        procViews += pv // not necessary to defer this to GUI because non-repainting happens in init!
      }
    }

    def removeProc(span: SpanLike, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      log(s"removeProcProc($span, $timed)")
      procMap.get(timed.id).foreach { pv =>
        procMap.remove(timed.id)
        guiFromTx {
          procViews -= pv
          pv.release()
          view.canvasComponent.repaint() // XXX TODO: optimize dirty rectangle
        }
      }
    }

    // insignificant changes are ignored, therefore one can just move the span without the track
    // by using trackCh = Change(0,0), and vice versa
    def procMoved(timed: TimedProc[S], spanCh: Change[SpanLike], trackCh: Change[Int])(implicit tx: S#Tx): Unit =
      procMap.get(timed.id).foreach { pv =>
        guiFromTx {
          procViews  -= pv
          if (spanCh .isSignificant) pv.span  = spanCh .now
          if (trackCh.isSignificant) pv.track = trackCh.now
          procViews  += pv
          view.canvasComponent.repaint()  // XXX TODO: optimize dirty rectangle
        }
      }

    def procMuteChanged(timed: TimedProc[S], newMute: Boolean)(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      log(s"procMuteChanged(newMute = $newMute, view = $pvo")
      pvo.foreach { pv =>
        guiFromTx {
          pv.muted = newMute
          view.canvasComponent.repaint()  // XXX TODO: optimize dirty rectangle
        }
      }
    }

    def procFadeChanged(timed: TimedProc[S], newFadeIn: FadeSpec.Value, newFadeOut: FadeSpec.Value)(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      log(s"procFadeChanged(newFadeIn = $newFadeIn, newFadeOut = $newFadeOut, view = $pvo")
      pvo.foreach { pv =>
        guiFromTx {
          pv.fadeIn   = newFadeIn
          pv.fadeOut  = newFadeOut
          view.canvasComponent.repaint()  // XXX TODO: optimize dirty rectangle
        }
      }
    }

    def procAudioChanged(timed: TimedProc[S], newAudio: Option[Grapheme.Segment.Audio])(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      log(s"procAudioChanged(newAudio = $newAudio, view = $pvo")
      pvo.foreach { pv =>
        guiFromTx {
          val newSono = (pv.audio, newAudio) match {
            case (Some(_), None)  => true
            case (None, Some(_))  => true
            case (Some(oldG), Some(newG)) if oldG.value.artifact != newG.value.artifact => true
            case _                => false
          }

          pv.audio = newAudio
          if (newSono) pv.release()
          view.canvasComponent.repaint()  // XXX TODO: optimize dirty rectangle
        }
      }
    }

    private def performDrop(drop: DnD.Drop[S]): Boolean = {
      drop.drag match {
        case ad: DnD.AudioDrag[S] =>
          step { implicit tx =>
            val group = groupH()
            group.modifiableOption match {
              case Some(groupM) =>
                InsertAudioRegion(groupM, time = drop.frame, track = view.screenToTrack(drop.y),
                   /* document = ad.document, */ grapheme = ad.source().entity, selection = ad.selection, bus = ad.bus)
                true
              case _ => false
            }
          }

        case id: DnD.IntDrag[S] =>
          view.findRegion(drop.frame, view.screenToTrack(drop.y)) match {
            case Some(hitRegion) =>
              val regions = if (selectionModel.contains(hitRegion)) selectionModel.iterator.toList else hitRegion :: Nil
              step { implicit tx =>
                val intElem = id.source()
                val attr    = Attribute.Int(intElem.entity)
                regions.foreach { r =>
                  val proc    = r.procSource()
                  proc.attributes.put(ProcKeys.attrBus, attr)
                }
              }
              true
            case _ => false
          }

        case _ => false
      }
    }

    private final class View extends ProcCanvasImpl[S] {
      view =>
      // import AbstractTimelineView._
      def timelineModel             = impl.timelineModel
      def selectionModel            = impl.selectionModel
      def group(implicit tx: S#Tx)  = impl.groupH()

      def intersect(span: Span): Iterator[ProcView[S]] = procViews.filterOverlaps((span.start, span.stop))

      def screenToTrack(y    : Int): Int = y     / 32
      def trackToScreen(track: Int): Int = track * 32

      def findRegion(pos: Long, hitTrack: Int): Option[ProcView[S]] = {
        val span      = Span(pos, pos + 1)
        val regions   = intersect(span)
        regions.find(pv => pv.track == hitTrack || (pv.track + 1) == hitTrack)
      }

      protected def commitToolChanges(value: Any): Unit = {
        log(s"Commit tool changes $value")
        step { implicit tx =>
          value match {
            case t: TrackTool.Move      => toolMove     commit t
            case t: TrackTool.Resize    => toolResize   commit t
            case t: TrackTool.Gain      => toolGain     commit t
            case t: TrackTool.Mute      => toolMute     commit t
            case t: TrackTool.Fade      => toolFade     commit t
            case t: TrackTool.Function  => toolFunction commit t
            case _ =>
          }
        }
      }

      private var _toolState    = Option.empty[Any]
      private var moveState     = NoMove
      private var resizeState   = NoResize
      private var gainState     = NoGain
      private var fadeState     = NoFade
      private var functionState = NoFunction

      protected def toolState = _toolState
      protected def toolState_=(state: Option[Any]): Unit = {
        _toolState    = state
        moveState     = NoMove
        resizeState   = NoResize
        gainState     = NoGain
        fadeState     = NoFade
        functionState = NoFunction
        state.foreach {
          case s: TrackTool.Move      => moveState      = s
          case s: TrackTool.Resize    => resizeState    = s
          case s: TrackTool.Gain      => gainState      = s
          case s: TrackTool.Fade      => fadeState      = s
          case s: TrackTool.Function  => functionState  = s
          case _ =>
        }
      }

      object canvasComponent extends Component with DnD[S] with sonogram.PaintController {
        protected def timelineModel = impl.timelineModel
        protected def document      = impl.document

        private var currentDrop = Option.empty[DnD.Drop[S]]

        // var visualBoost = 1f
        private var sonoBoost = 1f
        // private var regionViewMode: RegionViewMode = RegionViewMode.TitledBox

        font = {
          val f = UIManager.getFont("Slider.font", Locale.US)
          if (f != null) f.deriveFont(math.min(f.getSize2D, 9.5f)) else new Font("SansSerif", Font.PLAIN, 9)
        }
        // setOpaque(true)

        preferredSize = {
          val b = GUI.maximumWindowBounds
          (b.width >> 1, b.height >> 1)
        }

        protected def updateDnD(drop: Option[DnD.Drop[S]]): Unit = {
          currentDrop = drop
          repaint()
        }

        protected def acceptDnD(drop: DnD.Drop[S]): Boolean =
          performDrop(drop)

        def imageObserver = peer

        def adjustGain(amp: Float, pos: Double) = amp * sonoBoost

        private val shpFill = new Path2D.Float()
        private val shpDraw = new Path2D.Float()

        private def adjustFade(in: Curve, deltaCurve: Float): Curve = in match {
          case Curve.linear                 => Curve.parametric(math.max(-20, math.min(20,             deltaCurve)))
          case Curve.parametric(curvature)  => Curve.parametric(math.max(-20, math.min(20, curvature + deltaCurve)))
          case other                        => other
        }

        override protected def paintComponent(g: Graphics2D): Unit = {
          super.paintComponent(g)
          val w = peer.getWidth
          val h = peer.getHeight
          g.setColor(Color.darkGray) // g.setPaint(pntChecker)
          g.fillRect(0, 0, w, h)

          val total     = timelineModel.bounds
          // val visi      = timelineModel.visible
          val clipOrig  = g.getClip
          val cr        = clipOrig.getBounds
          val visiStart = screenToFrame(cr.x).toLong
          val visiStop  = screenToFrame(cr.x + cr.width).toLong

          val regionViewMode  = trackTools.regionViewMode
          val visualBoost     = trackTools.visualBoost
          val fadeViewMode    = trackTools.fadeViewMode
          // val fadeAdjusting   = fadeState != NoFade

          val hndl = regionViewMode match {
             case RegionViewMode.None       => 0
             case RegionViewMode.Box        => 1
             case RegionViewMode.TitledBox  => hndlExtent
          }

          val sel         = selectionModel

          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

          procViews.filterOverlaps((visiStart, visiStop)).foreach { pv =>
            val selected  = sel.contains(pv)

            def drawProc(start: Long, x1: Int, x2: Int, move: Long): Unit = {
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

                val innerH  = ph - (hndl + 1)
                val innerY  = py + hndl
                g.clipRect(px + 1, innerY, pw - 2, innerH)

                // --- sonagram ---
                pv.audio.foreach { segm =>
                  val sonoOpt = pv.sono.orElse(pv.acquire())

                  sonoOpt.foreach { sono =>
                    val audio   = segm.value
                    val dStart  = audio.offset /* - start */ + (/* start */ - segm.span.start) - move
                    val startC  = screenToFrame(px1C) // math.max(0.0, screenToFrame(px1C))
                    val stopC   = screenToFrame(px2C)
                    val boost   = if (selected) visualBoost * gainState.factor else visualBoost
                    // println(s"audio.gain = ${audio.gain.toFloat}")
                    sonoBoost   = audio.gain.toFloat * boost
                    val startP  = math.max(0L, startC + dStart)
                    val stopP   = startP + (stopC - startC)
                    sono.paint(startP, stopP, g, px1C, innerY, px2C - px1C, innerH, this)
                  }
                }

                // --- fades ---
                def paintFade(curve: Curve, fw: Float, y1: Float, y2: Float, x: Float, x0: Float): Unit = {
                  import math.{max, log10}
                  shpFill.reset()
                  shpDraw.reset()
                  val vscale  = innerH / -3f
                  val y1s     = max(-3, log10(y1)) * vscale + innerY
                  shpFill.moveTo(x, y1s)
                  shpDraw.moveTo(x, y1s)
                  var xs = 4
                  while (xs < fw) {
                    val ys = max(-3, log10(curve.levelAt(xs / fw, y1, y2))) * vscale + innerY
                    shpFill.lineTo(x + xs, ys)
                    shpDraw.lineTo(x + xs, ys)
                    xs += 3
                  }
                  val y2s     = max(-3, log10(y2)) * vscale + innerY
                  shpFill.lineTo(x + fw, y2s)
                  shpDraw.lineTo(x + fw, y2s)
                  shpFill.lineTo(x0, innerY)
                  g.setPaint(pntFade)
                  g.fill    (shpFill)
                  g.setColor(colrFade)
                  g.draw    (shpDraw)
                }

                if (fadeViewMode == FadeViewMode.Curve) {
                  val st      = if (selected) fadeState else NoFade
                  val fdIn    = pv.fadeIn  // XXX TODO: continue here. add delta
                  val fdInFr  = fdIn.numFrames + st.deltaFadeIn
                  if (fdInFr > 0) {
                    val fw    = framesToScreen(fdInFr).toFloat
                    val fdC   = st.deltaFadeInCurve
                    val shape = if (fdC != 0f) adjustFade(fdIn.curve, fdC) else fdIn.curve
                    // if (DEBUG) println(s"fadeIn. fw = $fw, shape = $shape, x = $px")
                    paintFade(shape, fw = fw, y1 = fdIn.floor, y2 = 1f, x = px, x0 = px)
                  }
                  val fdOut   = pv.fadeOut
                  val fdOutFr = fdOut.numFrames + st.deltaFadeOut
                  if (fdOutFr > 0) {
                    val fw    = framesToScreen(fdOutFr).toFloat
                    val fdC   = st.deltaFadeOutCurve
                    val shape = if (fdC != 0f) adjustFade(fdOut.curve, fdC) else fdOut.curve
                    // if (DEBUG) println(s"fadeIn. fw = $fw, shape = $shape, x = ${px + pw - 1 - fw}")
                    val x0    = px + pw - 1
                    paintFade(shape, fw = fw, y1 = 1f, y2 = fdOut.floor, x = x0 - fw, x0 = x0)
                  }
                }
                
                g.setClip(clipOrig)
    
                // --- label ---
                if (regionViewMode == RegionViewMode.TitledBox) {
                  val name = pv.name // .orElse(pv.audio.map(_.value.artifact.nameWithoutExtension))
                  g.clipRect(px + 2, py + 2, pw - 4, ph - 4)
                  g.setColor(Color.white)
                  // possible unicodes: 2327 23DB 24DC 25C7 2715 29BB
                  g.drawString(if (pv.muted) "\u23DB " + name else name, px + 4, py + hndlBaseline)
                  //              stakeInfo(ar).foreach { info =>
                  //                g2.setColor(Color.yellow)
                  //                g2.drawString(info, x + 4, y + hndlBaseline + hndlExtent)
                  //              }
                  g.setClip(clipOrig)
                }

                if (pv.muted) {
                  g.setColor(colrBgMuted)
                  g.fillRoundRect(px, py, pw, ph, 5, 5)
                }
              }
            }

            def adjustStart(start: Long): Long =
              if (selected) {
                val dt0 = moveState.deltaTime + resizeState.deltaStart
                if (dt0 >= 0) dt0 else {
                  math.max(-(start - total.start), dt0)
                }
              } else 0L

            def adjustStop(stop: Long): Long =
              if (selected) {
                val dt0 = moveState.deltaTime + resizeState.deltaStop
                if (dt0 >= 0) dt0 else {
                  math.max(-(stop - total.start + MinDur), dt0)
                }
              } else 0L

            def adjustMove(start: Long): Long =
              if (selected) {
                val dt0 = moveState.deltaTime
                if (dt0 >= 0) dt0 else {
                  math.max(-(start - total.start), dt0)
                }
              } else 0L

            pv.span match {
              case Span(start, stop) =>
                val dStart    = adjustStart(start)
                val dStop     = adjustStop (stop )
                val newStart  = start + dStart
                val newStop   = math.max(newStart + MinDur, stop + dStop)
                val x1        = frameToScreen(newStart).toInt
                val x2        = frameToScreen(newStop ).toInt
                drawProc(start, x1, x2, adjustMove(start))

              case Span.From(start) =>
                val dStart    = adjustStart(start)
                val newStart  = start + dStart
                val x1        = frameToScreen(newStart).toInt
                drawProc(start, x1, w + 5, adjustMove(start))

              case Span.Until(stop) =>
                val dStop     = adjustStop(stop)
                val newStop   = stop + dStop
                val x2        = frameToScreen(newStop).toInt
                drawProc(Long.MinValue, -5, x2, 0L)

              case Span.All =>
                drawProc(Long.MinValue, -5, w + 5, 0L)

              case _ => // don't draw Span.Void
            }
          }

          paintPosAndSelection(g, h)

          if (currentDrop.isDefined) currentDrop.foreach { drop =>
            drop.drag match {
              case ad: DnD.AudioDrag[S] =>
                val track = screenToTrack(drop.y)
                val span  = Span(drop.frame, drop.frame + ad.selection.length)
                drawDropFrame(g, track, span)

              case _ =>
            }
          }

          if (functionState.track >= 0) {
            drawDropFrame(g, functionState.track, functionState.span)
          }
        }

        private def drawDropFrame(g: Graphics2D, track: Int, span: Span): Unit = {
          val x1 = frameToScreen(span.start).toInt
          val x2 = frameToScreen(span.stop ).toInt
          g.setColor(colrDropRegionBg)
          val strkOrig = g.getStroke
          g.setStroke(strkDropRegion)
          val y   = trackToScreen(track)
          val x1b = math.min(x1 + 1, x2)
          val x2b = math.max(x1b, x2 - 1)
          g.drawRect(x1b, y + 1, x2b - x1b, 64)
          g.setStroke(strkOrig)
        }
      }
    }
  }
}