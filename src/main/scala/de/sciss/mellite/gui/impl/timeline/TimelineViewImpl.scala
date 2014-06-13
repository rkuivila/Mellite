/*
 *  ViewImpl.scala
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

package de.sciss.mellite
package gui
package impl
package timeline

import scala.swing.{Slider, Action, BorderPanel, Orientation, BoxPanel, Component, SplitPane}
import de.sciss.span.{Span, SpanLike}
import java.awt.{Rectangle, TexturePaint, Font, RenderingHints, BasicStroke, Color, Graphics2D, LinearGradientPaint}
import de.sciss.synth
import de.sciss.desktop
import de.sciss.lucre.stm
import de.sciss.sonogram
import de.sciss.lucre.stm.{Disposable, Cursor}
import de.sciss.synth.{Curve, proc}
import de.sciss.fingertree.RangedSeq
import javax.swing.UIManager
import java.util.Locale
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.audiowidgets.TimelineModel
import scala.swing.Swing._
import de.sciss.desktop.{KeyStrokes, Window}
import scala.concurrent.stm.Ref
import de.sciss.lucre.expr.Expr
import java.awt.geom.Path2D
import java.awt.image.BufferedImage
import scala.swing.event.{Key, ValueChanged}
import de.sciss.synth.proc.{Transport, ProcGroupElem, Obj, ExprImplicits, FadeSpec, AuralPresentation, Grapheme, ProcKeys, Proc, Scan, ProcGroup, TimedProc}
import de.sciss.audiowidgets.impl.TimelineModelImpl
import java.awt.geom.GeneralPath
import de.sciss.synth.io.AudioFile
import scala.util.Try
import de.sciss.model.Change
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.icons.raphael

object TimelineViewImpl {
  private val colrBg              = Color.darkGray
  private val colrDropRegionBg    = new Color(0xFF, 0xFF, 0xFF, 0x7F)
  private val strkDropRegion      = new BasicStroke(3f)
  private val colrRegionOutline   = new Color(0x68, 0x68, 0x68)
  private val colrRegionOutlineSel= Color.blue
  private val pntRegionBg         = new LinearGradientPaint(0f, 1f, 0f, 62f,
    Array[Float](0f, 0.23f, 0.77f, 1f), Array[Color](new Color(0x5E, 0x5E, 0x5E), colrRegionOutline,
      colrRegionOutline, new Color(0x77, 0x77, 0x77)))
  private val pntRegionBgSel       = new LinearGradientPaint(0f, 1f, 0f, 62f,
    Array[Float](0f, 0.23f, 0.77f, 1f), Array[Color](new Color(0x00, 0x00, 0xE6), colrRegionOutlineSel,
      colrRegionOutlineSel, new Color(0x1A, 0x1A, 0xFF)))
  private val colrRegionBgMuted   = new Color(0xFF, 0xFF, 0xFF, 0x60)
  private val colrLink            = new Color(0x80, 0x80, 0x80)
  private val strkLink            = new BasicStroke(2f)
  private val colrNameShadow      = new Color(0, 0, 0, 0x80)
  private val colrName            = Color.white
  private val path2d              = new GeneralPath

  private final val LinkArrowLen  = 0 // 10  ; currently no arrow tip painted
 	private final val LinkCtrlPtLen = 20

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

  private val DEBUG       = false

  import de.sciss.mellite.{logTimeline => logT}

  def apply[S <: Sys[S]](obj: Obj.T[S, ProcGroupElem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): TimelineView[S] = {
    val sampleRate  = 44100.0 // XXX TODO
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 60 * 60).toLong), sampleRate)
    tlm.visible     = Span(0L, (sampleRate * 60 * 2).toLong)
    val group       = obj.elem.peer
    import ProcGroup.serializer
    val groupH      = tx.newHandle(group)
    val groupEH     = tx.newHandle(obj  )

    var disposables = List.empty[Disposable[S#Tx]]

    // XXX TODO --- should use TransportView now!

    import workspace.inMemoryBridge // {cursor, inMemory}
    val procMap   = tx.newInMemoryIDMap[ProcView[S]]
    val scanMap   = tx.newInMemoryIDMap[(String, stm.Source[S#Tx, S#ID])]

    // ugly: the view dispose method cannot iterate over the procs
    // (other than through a GUI driven data structure). thus, it
    // only call pv.disposeGUI() and the procMap and scanMap must be
    // freed directly...
    disposables ::= procMap
    disposables ::= scanMap
    val transport = proc.Transport[S, workspace.I](group, sampleRate = sampleRate)
    disposables ::= transport
    val auralView = proc.AuralPresentation.run[S](transport, Mellite.auralSystem, Some(Mellite.sensorSystem))
    disposables ::= auralView

    val procSelectionModel = ProcSelectionModel[S]
    val global  = GlobalProcsView(workspace.folder, group, procSelectionModel)
    disposables ::= global

    val transportView = TransportView(transport, tlm, hasMillis = true, hasLoop = true)

    val view    = new Impl(workspace, groupH, groupEH, transport, procMap, scanMap, tlm, auralView, global,
      transportView, procSelectionModel)

    group.iterator.foreach { case (span, seq) =>
      seq.foreach { timed =>
        view.addProc(span, timed, repaint = false)
      }
    }

    def muteChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val muted   = attr.expr[Boolean](ProcKeys.attrMute).exists(_.value)
      view.procMuteChanged(timed, muted)
    }

    def nameChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val nameOpt = attr.expr[String](ProcKeys.attrName).map(_.value)
      view.procNameChanged(timed, nameOpt)
    }

    def gainChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr  = timed.value.attr
      val gain  = attr.expr[Double](ProcKeys.attrGain).fold(1.0)(_.value)
      view.procGainChanged(timed, gain)
    }

    def busChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val busOpt  = attr.expr[Int](ProcKeys.attrBus).map(_.value)
      view.procBusChanged(timed, busOpt)
    }

    def fadeChanged(timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val fadeIn  = attr.expr[FadeSpec](ProcKeys.attrFadeIn ).fold(TrackTool.EmptyFade)(_.value)
      val fadeOut = attr.expr[FadeSpec](ProcKeys.attrFadeOut).fold(TrackTool.EmptyFade)(_.value)
      view.procFadeChanged(timed, fadeIn, fadeOut)
    }

    def attrChanged(timed: TimedProc[S], name: String)(implicit tx: S#Tx): Unit =
      name match {
        case ProcKeys.attrMute  => muteChanged(timed)
        case ProcKeys.attrFadeIn | ProcKeys.attrFadeOut => fadeChanged(timed)
        case ProcKeys.attrName  => nameChanged(timed)
        case ProcKeys.attrGain  => gainChanged(timed)
        case ProcKeys.attrBus   => busChanged (timed)
        case _ =>
      }

    def scanAdded(timed: TimedProc[S], name: String)(implicit tx: S#Tx): Unit = {
      val proc = timed.value.elem.peer
      proc.scans.get(name).foreach { scan =>
        scan.sources.foreach {
          case Scan.Link.Scan(peer) =>
            view.scanSourceAdded(timed, name, scan, peer)
          case _ =>
        }
        scan.sinks.foreach {
          case Scan.Link.Scan(peer) =>
            view.scanSinkAdded(timed, name, scan, peer)
          case _ =>
        }
      }
    }

    def scanRemoved(timed: TimedProc[S], name: String)(implicit tx: S#Tx): Unit = {
      val proc = timed.value.elem.peer
      proc.scans.get(name).foreach { scan =>
        scan.sources.foreach {
          case Scan.Link.Scan(peer) =>
            view.scanSourceRemoved(timed, name, scan, peer)
          case _ =>
        }
        scan.sinks.foreach {
          case Scan.Link.Scan(peer) =>
            view.scanSinkRemoved(timed, name, scan, peer)
          case _ =>
        }
      }
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
          case Obj.ElemChange(updP) =>
            updP.changes.foreach {
              case Proc.ScanAdded  (key, _) => scanAdded  (timed, key)
              case Proc.ScanRemoved(key, _) => scanRemoved(timed, key)
              case Proc.ScanChange(name, scan, scanUpdates) =>
                scanUpdates.foreach {
                  case Scan.GraphemeChange(grapheme, segments) =>
                    if (name == ProcKeys.graphAudio) {
                      timed.span.value match {
                        case Span.HasStart(startFrame) =>
                          val segmOpt = segments.find(_.span.contains(startFrame)) match {
                            case Some(segm: Grapheme.Segment.Audio) => Some(segm)
                            case _ => None
                          }
                          view.procAudioChanged(timed, segmOpt)
                        case _ =>
                      }
                    }

                  case Scan.SinkAdded    (Scan.Link.Scan(peer)) =>
                    val test: Scan[S] = scan
                    view.scanSinkAdded    (timed, name, test, peer)
                  case Scan.SinkRemoved  (Scan.Link.Scan(peer)) => view.scanSinkRemoved  (timed, name, scan, peer)
                  case Scan.SourceAdded  (Scan.Link.Scan(peer)) => view.scanSourceAdded  (timed, name, scan, peer)
                  case Scan.SourceRemoved(Scan.Link.Scan(peer)) => view.scanSourceRemoved(timed, name, scan, peer)

                  case _ => // Scan.SinkAdded(_) | Scan.SinkRemoved(_) | Scan.SourceAdded(_) | Scan.SourceRemoved(_)
                }
              case Proc.GraphChange(_)      =>
            }

          case Obj.AttrAdded  (key, _) => attrChanged(timed, key)
          case Obj.AttrRemoved(key, _) => attrChanged(timed, key)

          case Obj.AttrChange(name, attr, ach) =>
            (name, ach) match {
              case (ProcKeys.attrTrack, changes) =>
                changes.foreach {
                  case Obj.ElemChange(Change(before: Int, now: Int)) =>
                    view.procMoved(timed, spanCh = Change(Span.Void, Span.Void), trackCh = Change(before, now))
                  case _ =>
                }

              case _ => attrChanged(timed, name)
            }
        }
    }}
    disposables ::= obsGroup

    view.disposables.set(disposables)(tx.peer)

    deferTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](val workspace     : Workspace[S],
                                        groupH            : stm.Source[S#Tx, proc.ProcGroup[S]],
                                        groupEH           : stm.Source[S#Tx, Obj.T[S, ProcGroupElem]],
                                        transport         : Transport.Realtime[S, Obj.T[S, Proc.Elem], Transport.Proc.Update[S]],
                                        procMap           : ProcView.ProcMap[S],
                                        scanMap           : ProcView.ScanMap[S],
                                        val timelineModel : TimelineModel,
                                        auralView         : AuralPresentation[S],
                                        globalView        : GlobalProcsView[S],
                                        transportView     : TransportView[S],
                                        val procSelectionModel: ProcSelectionModel[S])
                                       (implicit val cursor: Cursor[S])
    extends TimelineView[S] with ComponentHolder[Component] {
    impl =>

    import cursor.step

    private var procViews = RangedSeq.empty[ProcView[S], Long]

    private var view: View    = _
    val disposables           = Ref(List.empty[Disposable[S#Tx]])

    private lazy val toolCursor   = TrackTool.cursor  [S](view)
    private lazy val toolMove     = TrackTool.move    [S](view)
    private lazy val toolResize   = TrackTool.resize  [S](view)
    private lazy val toolGain     = TrackTool.gain    [S](view)
    private lazy val toolMute     = TrackTool.mute    [S](view)
    private lazy val toolFade     = TrackTool.fade    [S](view)
    private lazy val toolFunction = TrackTool.function[S](view)
    private lazy val toolPatch    = TrackTool.patch   [S](view)

    def group     (implicit tx: S#Tx) = groupEH()
    def plainGroup(implicit tx: S#Tx) = groupH()

    def window: Window = component.peer.getClientProperty("de.sciss.mellite.Window").asInstanceOf[Window]

    def dispose()(implicit tx: S#Tx): Unit = {
      disposables.swap(Nil)(tx.peer).foreach(_.dispose())
      deferTx {
        // DocumentViewHandler.instance.remove(this)
        val pit     = procViews.iterator
        procViews   = RangedSeq.empty
        pit.foreach(_.disposeGUI())
        procMap.dispose()
        scanMap.dispose()
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
        val (_settings, ok) = query(setUpd, workspace, timelineModel, window = window)
        settings = _settings
        _settings.file match {
          case Some(file) if ok =>
            performGUI(workspace, _settings, groupH, file, window = window)
          case _ =>
        }
      }
    }

    object deleteAction extends Action("Delete") {
      def apply(): Unit =
        withSelection { implicit tx => views =>
          plainGroup.modifiableOption.foreach { mod =>
            ProcGUIActions.removeProcs(mod, views)
          }
        }
    }

    object splitObjectsAction extends Action("Split Selected Objects") {
      import KeyStrokes.menu2
      accelerator = Some(menu2 + Key.Y)

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
      val sel = procSelectionModel.iterator
      if (sel.hasNext) step { implicit tx => fun(tx)(sel) }
    }

    private def withFilteredSelection(p: ProcView[S] => Boolean)
                                     (fun: S#Tx => TraversableOnce[ProcView[S]] => Unit): Unit = {
      val sel = procSelectionModel.iterator
      val flt = sel.filter(p)
      if (flt.hasNext) step { implicit tx => fun(tx)(flt) }
    }

    private def debugCheckConsistency(info: => String)(implicit tx: S#Tx): Unit = if (DEBUG) {
      val check = BiGroupImpl.verifyConsistency(plainGroup, reportOnly = true)
      check.foreach { msg =>
        println(info)
        println(msg)
        sys.error("Rollback")
      }
    }

    def splitObjects(time: Long)(views: TraversableOnce[ProcView[S]])(implicit tx: S#Tx): Unit =
      for {
        group             <- plainGroup.modifiableOption
        pv                <- views
      } {
        pv.spanSource() match {
          case Expr.Var(oldSpan) =>
            val imp = ExprImplicits[S]
            import imp._
            val leftProc  = pv.proc
            val rightProc = ProcActions.copy[S](leftProc, Some(oldSpan))
            val oldVal    = oldSpan.value
            val rightSpan = oldVal match {
              case Span.HasStart(leftStart) =>
                val _rightSpan  = SpanLikeEx.newVar(oldSpan())
                val resize      = ProcActions.Resize(time - leftStart, 0L)
                ProcActions.resize(_rightSpan, rightProc, resize, timelineModel)
                _rightSpan

              case Span.HasStop(rightStop) =>
                SpanLikeEx.newVar(Span(time, rightStop))
            }

            oldVal match {
              case Span.HasStop(rightStop) =>
                val resize = ProcActions.Resize(0L, time - rightStop)
                ProcActions.resize(oldSpan, leftProc, resize, timelineModel)

              case Span.HasStart(leftStart) =>
                val leftSpan = Span(leftStart, time)
                oldSpan() = leftSpan
            }

            group.add(rightSpan, rightProc)
            debugCheckConsistency(s"Split left = $leftProc, oldSpan = $oldVal; right = $rightProc, rightSpan = ${rightSpan.value}")

          case _ =>
        }
      }

    def guiInit(): Unit = {
      import desktop.Implicits._

      view = new View
      val ggVisualBoost = new Slider {
        min       = 0
        max       = 64
        value     = 0
        focusable = false
        peer.putClientProperty("JComponent.sizeVariant", "small")
        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            import synth._
            view.trackTools.visualBoost = value.linexp(0, 64, 1, 512) // .toFloat
        }
      }
      GUI.fixWidth(ggVisualBoost)

      val actionAttr: Action = Action(null) {
        withSelection { implicit tx =>
          seq => seq.foreach { view =>
            AttrMapFrame(workspace, view.proc)
          }
        }
      }
      val ggAttr = GUI.toolButton(actionAttr, raphael.Shapes.Wrench, "Attributes Editor")
      ggAttr.focusable = false

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          TrackTools.palette(view.trackTools, Vector(
            toolCursor, toolMove, toolResize, toolGain, toolFade /* , toolSlide*/ ,
            toolMute /* , toolAudition */, toolFunction, toolPatch)),
          HStrut(4),
          ggAttr,
          HStrut(4),
          ggVisualBoost,
          HGlue,
          HStrut(4),
          transportView.component,
          HStrut(4)
        )
      }

      val pane2 = new SplitPane(Orientation.Vertical, globalView.component, view.component)
      pane2.dividerSize = 4
      pane2.border      = null

      val pane = new BorderPanel {
        import BorderPanel.Position._
        layoutManager.setVgap(2)
        add(transportPane, North )
        add(pane2        , Center)
        // add(globalView.component, West  )
      }

      component = pane
      // DocumentViewHandler.instance.add(this)
    }

    private def repaintAll(): Unit = view.canvasComponent.repaint()

    def addProc(span: SpanLike, timed: TimedProc[S], repaint: Boolean)(implicit tx: S#Tx): Unit = {
      logT(s"addProc($span, $timed)")
      // timed.span
      // val proc = timed.value
      val pv = ProcView(timed, procMap, scanMap)

      def doAdd(): Unit =
        if (pv.isGlobal) {
          globalView.add(pv)
        } else {
          procViews += pv
          if (repaint) repaintAll()    // XXX TODO: optimize dirty rectangle
        }

      if (repaint)
        deferTx(doAdd())
      else
        doAdd()
    }

    def removeProc(span: SpanLike, timed: TimedProc[S])(implicit tx: S#Tx): Unit = {
      logT(s"removeProcProc($span, $timed)")
      procMap.get(timed.id).foreach { pv =>
        pv.disposeTx(timed, procMap, scanMap)
        deferTx {
          if (pv.isGlobal)
            globalView.remove(pv)
          else
            procViews -= pv

          pv.disposeGUI()

          if (!pv.isGlobal) repaintAll() // XXX TODO: optimize dirty rectangle
        }
      }
    }

    // insignificant changes are ignored, therefore one can just move the span without the track
    // by using trackCh = Change(0,0), and vice versa
    def procMoved(timed: TimedProc[S], spanCh: Change[SpanLike], trackCh: Change[Int])(implicit tx: S#Tx): Unit =
      procMap.get(timed.id).foreach { pv =>
        deferTx {
          if (pv.isGlobal)
            globalView.remove(pv)
          else
            procViews -= pv

          if (spanCh .isSignificant) pv.span  = spanCh .now
          if (trackCh.isSignificant) pv.track = trackCh.now

          if (pv.isGlobal) {
            globalView.add(pv)
          } else {
            procViews += pv
            repaintAll()  // XXX TODO: optimize dirty rectangle
          }
        }
      }

    private def procUpdated(view: ProcView[S]): Unit =
      if (view.isGlobal)
        globalView.updated(view)
      else
        repaintAll()  // XXX TODO: optimize dirty rectangle

    def procMuteChanged(timed: TimedProc[S], newMute: Boolean)(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      logT(s"procMuteChanged(newMute = $newMute, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          pv.muted = newMute
          procUpdated(pv)
        }
      }
    }

    def procNameChanged(timed: TimedProc[S], newName: Option[String])(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      logT(s"procNameChanged(newName = $newName, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          pv.nameOption = newName
          procUpdated(pv)
        }
      }
    }

    def procBusChanged(timed: TimedProc[S], newBus: Option[Int])(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      logT(s"procBusChanged(newBus = $newBus, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          pv.busOption = newBus
          procUpdated(pv)
        }
      }
    }

    def procGainChanged(timed: TimedProc[S], newGain: Double)(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      logT(s"procGainChanged(newGain = $newGain, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          pv.gain = newGain
          procUpdated(pv)
        }
      }
    }

    def procFadeChanged(timed: TimedProc[S], newFadeIn: FadeSpec, newFadeOut: FadeSpec)(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      logT(s"procFadeChanged(newFadeIn = $newFadeIn, newFadeOut = $newFadeOut, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          pv.fadeIn   = newFadeIn
          pv.fadeOut  = newFadeOut
          repaintAll()  // XXX TODO: optimize dirty rectangle
        }
      }
    }

    def procAudioChanged(timed: TimedProc[S], newAudio: Option[Grapheme.Segment.Audio])(implicit tx: S#Tx): Unit = {
      val pvo = procMap.get(timed.id)
      logT(s"procAudioChanged(newAudio = $newAudio, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          val newSonogram = (pv.audio, newAudio) match {
            case (Some(_), None)  => true
            case (None, Some(_))  => true
            case (Some(oldG), Some(newG)) if oldG.value.artifact != newG.value.artifact => true
            case _                => false
          }

          pv.audio = newAudio
          if (newSonogram) pv.releaseSonogram()
          repaintAll()  // XXX TODO: optimize dirty rectangle
        }
      }
    }

    private def withLink(timed: TimedProc[S], that: Scan[S])(fun: (ProcView[S], ProcView[S], String) => Unit)
                        (implicit tx: S#Tx): Unit =
      for {
        thisView            <- procMap.get(timed.id)
        (thatKey, thatIdH)  <- scanMap.get(that .id)
        thatView            <- procMap.get(thatIdH())
      } {
        deferTx {
          fun(thisView, thatView, thatKey)
          repaintAll()
        }
      }

    def scanSinkAdded(timed: TimedProc[S], srcKey: String, src: Scan[S], sink: Scan[S])(implicit tx: S#Tx): Unit =
      withLink(timed, sink) { (srcView, sinkView, sinkKey) =>
        logT(s"scanSinkAdded(src-key = $srcKey, source = $src, sink = $sink, src-view = $srcView, sink-view $sinkView")
        srcView .addOutput(srcKey , sinkView, sinkKey)
        sinkView.addInput (sinkKey, srcView , srcKey )
      }

    def scanSinkRemoved(timed: TimedProc[S], srcKey: String, src: Scan[S], sink: Scan[S])(implicit tx: S#Tx): Unit =
      withLink(timed, sink) { (srcView, sinkView, sinkKey) =>
        logT(s"scanSinkRemoved(src-key = $srcKey, source = $src, sink = $sink, src-view = $srcView, sink-view $sinkView")
        srcView .removeOutput(srcKey , sinkView, sinkKey)
        sinkView.removeInput (sinkKey, srcView , srcKey )
      }

    def scanSourceAdded(timed: TimedProc[S], sinkKey: String, sink: Scan[S], src: Scan[S])(implicit tx: S#Tx): Unit =
      withLink(timed, src) { (sinkView, srcView, srcKey) =>
        logT(s"scanSourceAdded(src-key = $srcKey, source = $src, sink = $sink, src-view = $srcView, sink-view $sinkView")
        srcView .addOutput(srcKey , sinkView, sinkKey)
        sinkView.addInput (sinkKey, srcView , srcKey )
      }

    def scanSourceRemoved(timed: TimedProc[S], sinkKey: String, sink: Scan[S], src: Scan[S])(implicit tx: S#Tx): Unit =
      withLink(timed, sink) { (sinkView, srcView, srcKey) =>
        logT(s"scanSourceRemoved(src-key = $srcKey, source = $src, sink = $sink, src-view = $srcView, sink-view $sinkView")
        srcView .removeOutput(srcKey , sinkView, sinkKey)
        sinkView.removeInput (sinkKey, srcView , srcKey )
      }

    private def insertAudioRegion(drop: DnD.Drop[S], drag: DnD.AudioDragLike[S],
                                  grapheme: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): Boolean =
      plainGroup.modifiableOption match {
        case Some(groupM) =>
          ProcActions.insertAudioRegion(groupM, time = drop.frame, track = view.screenToTrack(drop.y),
            grapheme = grapheme, selection = drag.selection, bus = None) // , bus = ad.bus.map(_.apply().entity))
          true
        case _ => false
      }

    private def performDrop(drop: DnD.Drop[S]): Boolean = {
      def withRegions(fun: S#Tx => List[ProcView[S]] => Boolean): Boolean =
        view.findRegion(drop.frame, view.screenToTrack(drop.y)).exists { hitRegion =>
          val regions = if (procSelectionModel.contains(hitRegion)) procSelectionModel.iterator.toList else hitRegion :: Nil
          step { implicit tx =>
            fun(tx)(regions)
          }
        }

      // println(s"performDrop($drop)")

      drop.drag match {
        case ad: DnD.AudioDrag[S] =>
          step { implicit tx =>
            insertAudioRegion(drop, ad, ad.source().elem.peer)
          }

        case ed: DnD.ExtAudioRegionDrag[S] =>
          val file = ed.file
          val resOpt = step { implicit tx =>
            ObjectActions.findAudioFile(workspace.root(), file).map { grapheme =>
              insertAudioRegion(drop, ed, grapheme.elem.peer)
            }
          }

          resOpt.getOrElse {
            Try(AudioFile.readSpec(file)).toOption.fold(false) { spec =>
              ActionArtifactLocation.query[S](workspace.root, file).fold(false) { src =>
                step { implicit tx =>
                  src().elem.peer.modifiableOption.fold(false) { loc =>
                    val elems = workspace.root()
                    // val obj   = ObjectActions.addAudioFile(elems, elems.size, loc, file, spec)
                    val obj   = ObjectActions.mkAudioFile(loc, file, spec)
                    elems.addLast(obj)
                    insertAudioRegion(drop, ed, obj.elem.peer)
                  }
                }
              }
            }
          }

        case id: DnD.IntDrag[S] => withRegions { implicit tx => regions =>
          val intExpr = id.source().elem.peer
          ProcActions.setBus[S](regions.map(_.proc), intExpr)
          true
        }

        case cd: DnD.CodeDrag[S] => withRegions { implicit tx => regions =>
          val codeElem  = cd.source()
          ProcActions.setSynthGraph[S](regions.map(_.proc), codeElem)
        }

        case pd: DnD.ProcDrag[S] => withRegions { implicit tx => regions =>
          val in = pd.source()
          regions.map { pv =>
            val out = pv.proc
            ProcActions.linkOrUnlink[S](out, in)
          } .exists(identity)
        }

        case _ => false
      }
    }

    private final class View extends ProcCanvasImpl[S] {
      view =>
      // import AbstractTimelineView._
      def timelineModel             = impl.timelineModel
      def selectionModel            = impl.procSelectionModel
      def group(implicit tx: S#Tx)  = impl.plainGroup

      def intersect(span: Span): Iterator[ProcView[S]] = procViews.filterOverlaps((span.start, span.stop))

      def screenToTrack(y    : Int): Int = y     / 32
      def trackToScreen(track: Int): Int = track * 32

      def findRegion(pos: Long, hitTrack: Int): Option[ProcView[S]] = {
        val span      = Span(pos, pos + 1)
        val regions   = intersect(span)
        regions.find(pv => pv.track == hitTrack || (pv.track + 1) == hitTrack)
      }

      protected def commitToolChanges(value: Any): Unit = {
        logT(s"Commit tool changes $value")
        step { implicit tx =>
          value match {
            case t: TrackTool.Cursor    => toolCursor   commit t
            case t: TrackTool.Move      =>
              // println("\n----BEFORE----")
              // println(group.debugPrint)
              toolMove     commit t
              // println("\n----AFTER----")
              // println(group.debugPrint)
              debugCheckConsistency(s"Move $t")

            case t: TrackTool.Resize    =>
              toolResize   commit t
              debugCheckConsistency(s"Resize $t")

            case t: TrackTool.Gain      => toolGain     commit t
            case t: TrackTool.Mute      => toolMute     commit t
            case t: TrackTool.Fade      => toolFade     commit t
            case t: TrackTool.Function  => toolFunction commit t
            case t: TrackTool.Patch[S]  => toolPatch    commit t
            case _ =>
          }
        }
      }

      private val NoPatch       = TrackTool.Patch[S](null, null) // not cool
      private var _toolState    = Option.empty[Any]
      private var moveState     = NoMove
      private var resizeState   = NoResize
      private var gainState     = NoGain
      private var fadeState     = NoFade
      private var functionState = NoFunction
      private var patchState    = NoPatch

      protected def toolState = _toolState
      protected def toolState_=(state: Option[Any]): Unit = {
        _toolState    = state
        moveState     = NoMove
        resizeState   = NoResize
        gainState     = NoGain
        fadeState     = NoFade
        functionState = NoFunction
        patchState    = NoPatch

        state.foreach {
          case s: TrackTool.Move      => moveState      = s
          case s: TrackTool.Resize    => resizeState    = s
          case s: TrackTool.Gain      => gainState      = s
          case s: TrackTool.Fade      => fadeState      = s
          case s: TrackTool.Function  => functionState  = s
          case s: TrackTool.Patch[S]  => patchState     = s
          case _ =>
        }
      }

      object canvasComponent extends Component with DnD[S] with sonogram.PaintController {
        protected def timelineModel = impl.timelineModel
        protected def document      = impl.workspace.folder

        private var currentDrop = Option.empty[DnD.Drop[S]]

        // var visualBoost = 1f
        private var sonogramBoost = 1f
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

        protected def acceptDnD(drop: DnD.Drop[S]): Boolean = performDrop(drop)

        def imageObserver = peer

        def adjustGain(amp: Float, pos: Double) = amp * sonogramBoost

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
          g.setColor(colrBg) // g.setPaint(pntChecker)
          g.fillRect(0, 0, w, h)

          val total     = timelineModel.bounds
          // val visible    = timelineModel.visible
          val clipOrig  = g.getClip
          val strkOrig  = g.getStroke
          val cr        = clipOrig.getBounds
          val visStart  = screenToFrame(cr.x).toLong
          val visStop   = screenToFrame(cr.x + cr.width).toLong + 1 // plus one to avoid glitches

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

          val pvs = procViews.filterOverlaps((visStart, visStop)).toList // warning: iterator, we need to traverse twice!
          pvs.foreach { pv =>
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
                  g.translate(px, py)
                  g.setColor(if (selected) colrRegionOutlineSel else colrRegionOutline)
                  g.fillRoundRect(0, 0, pw, ph, 5, 5)
                  g.setPaint(if (selected) pntRegionBgSel else pntRegionBg)
                  g.fillRoundRect(1, 1, pw - 2, ph - 2, 4, 4)
                  g.translate(-px, -py)
                }
                g.setColor(colrBg)
                g.drawLine(px - 1, py, px - 1, py + ph - 1) // better distinguish directly neighbouring regions

                val innerH  = ph - (hndl + 1)
                val innerY  = py + hndl
                g.clipRect(px + 1, innerY, pw - 2, innerH)

                // --- sonogram ---
                pv.audio.foreach { segm =>
                  val sonogramOpt = pv.sonogram.orElse(pv.acquireSonogram())

                  sonogramOpt.foreach { sonogram =>
                    val audio   = segm.value
                    val dStart  = audio.offset /* - start */ + (/* start */ - segm.span.start) - move
                    val startC  = screenToFrame(px1C) // math.max(0.0, screenToFrame(px1C))
                    val stopC   = screenToFrame(px2C)
                    val boost   = if (selected) visualBoost * gainState.factor else visualBoost
                    // println(s"audio.gain = ${audio.gain.toFloat}")
                    sonogramBoost   = (audio.gain + pv.gain).toFloat * boost
                    val startP  = math.max(0L, startC + dStart)
                    val stopP   = startP + (stopC - startC)
                    sonogram.paint(startP, stopP, g, px1C, innerY, px2C - px1C, innerH, this)
                  }
                }

                // --- fades ---
                def paintFade(curve: Curve, fw: Float, y1: Float, y2: Float, x: Float, x0: Float): Unit = {
                  import math.{max, log10}
                  shpFill.reset()
                  shpDraw.reset()
                  val vScale  = innerH / -3f
                  val y1s     = max(-3, log10(y1)) * vScale + innerY
                  shpFill.moveTo(x, y1s)
                  shpDraw.moveTo(x, y1s)
                  var xs = 4
                  while (xs < fw) {
                    val ys = max(-3, log10(curve.levelAt(xs / fw, y1, y2))) * vScale + innerY
                    shpFill.lineTo(x + xs, ys)
                    shpDraw.lineTo(x + xs, ys)
                    xs += 3
                  }
                  val y2s     = max(-3, log10(y2)) * vScale + innerY
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
                  // possible unicodes: 2327 23DB 24DC 25C7 2715 29BB
                  val text  = if (pv.muted) "\u23DB " + name else name
                  val tx    = px + 4
                  val ty    = py + hndlBaseline
                  g.setColor(colrNameShadow)
                  g.drawString(text, tx, ty + 1)
                  g.setColor(colrName)
                  g.drawString(text, tx, ty)
                  //              stakeInfo(ar).foreach { info =>
                  //                g2.setColor(Color.yellow)
                  //                g2.drawString(info, x + 4, y + hndlBaseline + hndlExtent)
                  //              }
                  g.setClip(clipOrig)
                }

                if (pv.muted) {
                  g.setColor(colrRegionBgMuted)
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

          // --- links ---
          g.setColor(colrLink)
          g.setStroke(strkLink)

          pvs.foreach { pv =>
            // println(s"For ${pv.name} inputs = ${pv.inputs}, outputs = ${pv.outputs}")
            pv.outputs.foreach { case (_, links) =>
              links.foreach { link =>
                if (link.target.isGlobal) {
                  if (regionViewMode == RegionViewMode.TitledBox) {
                    // XXX TODO: extra info such as gain
                  }

                } else {
                  drawLink(g, pv, link.target)
                }
              }
            }
          }
          g.setStroke(strkOrig)

          // --- timeline cursor and selection ---
          paintPosAndSelection(g, h)

          // --- ongoing drag and drop / tools ---
          if (currentDrop.isDefined) currentDrop.foreach { drop =>
            drop.drag match {
              case ad: DnD.AudioDragLike[S] =>
                val track = screenToTrack(drop.y)
                val span  = Span(drop.frame, drop.frame + ad.selection.length)
                drawDropFrame(g, track, span)

              case _ =>
            }
          }

          if (functionState.track >= 0) {
            drawDropFrame(g, functionState.track, functionState.span)
          }

          if (patchState.source != null) {
            drawPatch(g, patchState)
          }
        }

        private def linkFrame(pv: ProcView[S]): Long = pv.span match {
          case Span(start, stop)  => (start + stop)/2
          case hs: Span.HasStart  => hs.start + (timelineModel.sampleRate * 0.1).toLong
          case _ => 0L
        }

        private def linkY(view: ProcView[S], input: Boolean): Int =
          if (input)
            trackToScreen(view.track) + 4
          else
            trackToScreen(view.track + 2) - 5

        private def drawLink(g: Graphics2D, source: ProcView[S], sink: ProcView[S]): Unit = {
          val srcFrameC   = linkFrame(source)
          val sinkFrameC  = linkFrame(sink)
          val srcY        = linkY(source, input = false)
          val sinkY       = linkY(sink  , input = true )

          drawLinkLine(g, srcFrameC, srcY, sinkFrameC, sinkY)
        }

        @inline private def drawLinkLine(g: Graphics2D, pos1: Long, y1: Int, pos2: Long, y2: Int): Unit = {
          val x1      = frameToScreen(pos1).toFloat
          val x2      = frameToScreen(pos2).toFloat
          // g.drawLine(x1, y1, x2, y2)

          // Yo crazy mama, Wolkenpumpe "5" style
          val ctrlLen = math.min(LinkCtrlPtLen, math.abs(y2 - LinkArrowLen - y1))
          path2d.reset()
          path2d.moveTo (x1 - 0.5f, y1)
          path2d.curveTo(x1 - 0.5f, y1 + ctrlLen, x2 - 0.5f, y2 - ctrlLen - LinkArrowLen, x2 - 0.5f, y2 - LinkArrowLen)
          g.draw(path2d)
        }

        private def drawPatch(g: Graphics2D, patch: TrackTool.Patch[S]): Unit = {
          val src       = patch.source
          val srcFrameC = linkFrame(src)
          val srcY      = linkY(src, input = false)
          val (sinkFrameC, sinkY) = patch.sink match {
            case TrackTool.Patch.Unlinked(f, y) => (f, y)
            case TrackTool.Patch.Linked(sink) =>
              val f       = linkFrame(sink)
              val y1      = linkY(sink, input = true)
              (f, y1)
          }

          g.setColor(colrDropRegionBg)
          val strkOrig = g.getStroke
          g.setStroke(strkDropRegion)
          drawLinkLine(g, srcFrameC, srcY, sinkFrameC, sinkY)
          g.setStroke(strkOrig)
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