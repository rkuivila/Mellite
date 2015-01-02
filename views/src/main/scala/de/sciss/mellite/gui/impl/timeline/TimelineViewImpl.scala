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

import javax.swing.undo.UndoableEdit

import de.sciss.desktop.edit.CompoundEdit
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.mellite.gui.edit.{EditFolderInsertObj, Edits, EditTimelineInsertObj}
import de.sciss.swingplus.ScrollBar

import de.sciss.span.{Span, SpanLike}
import de.sciss.synth
import de.sciss.desktop
import de.sciss.lucre.stm
import de.sciss.sonogram
import de.sciss.lucre.stm.{Disposable, Cursor}
import de.sciss.synth.proc.gui.TransportView
import de.sciss.synth.{Curve, proc}
import de.sciss.fingertree.RangedSeq
import de.sciss.lucre.bitemp.BiGroup
import de.sciss.audiowidgets.TimelineModel
import de.sciss.desktop.{UndoManager, KeyStrokes, Window}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.expr.{Int => IntEx}
import de.sciss.synth.proc.{BooleanElem, StringElem, DoubleElem, ObjKeys, IntElem, TimeRef, Timeline, Transport, Obj, ExprImplicits, FadeSpec, Grapheme, Proc, Scan}
import de.sciss.audiowidgets.impl.TimelineModelImpl
import de.sciss.synth.io.AudioFile
import de.sciss.model.Change
import de.sciss.lucre.bitemp.impl.BiGroupImpl
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.icons.raphael
import TimelineView.TrackScale

import scala.annotation.tailrec
import scala.swing.{Slider, Action, BorderPanel, Orientation, BoxPanel, Component, SplitPane}
import scala.swing.Swing._
import scala.swing.event.{Key, ValueChanged}
import scala.concurrent.stm.{TSet, Ref}
import scala.util.Try

import java.awt.{Rectangle, TexturePaint, Font, RenderingHints, BasicStroke, Color, Graphics2D, LinearGradientPaint}
import javax.swing.UIManager
import java.util.Locale
import java.awt.geom.Path2D
import java.awt.image.BufferedImage
import java.awt.geom.GeneralPath

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
  private val NoFunction  = TrackTool.Function(-1, -1, Span(0L, 0L))

  private val MinDur      = 32

  private val DEBUG       = false

  import de.sciss.mellite.{logTimeline => logT}

  private type TimedProc[S <: Sys[S]] = BiGroup.TimedElem[S, Proc.Obj[S]]

  def apply[S <: Sys[S]](obj: Timeline.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undo: UndoManager): TimelineView[S] = {
    val sampleRate  = Timeline.SampleRate
    val tlm         = new TimelineModelImpl(Span(0L, (sampleRate * 60 * 60).toLong), sampleRate)
    tlm.visible     = Span(0L, (sampleRate * 60 * 2).toLong)
    val group       = obj.elem.peer
    import Timeline.serializer
    val groupH      = tx.newHandle(group)
    val groupEH     = tx.newHandle(obj  )

    var disposables = List.empty[Disposable[S#Tx]]

    // XXX TODO --- should use TransportView now!

    import workspace.inMemoryBridge // {cursor, inMemory}
    val viewMap   = tx.newInMemoryIDMap[TimelineObjView[S]]
    val scanMap   = tx.newInMemoryIDMap[(String, stm.Source[S#Tx, S#ID])]

    // ugly: the view dispose method cannot iterate over the procs
    // (other than through a GUI driven data structure). thus, it
    // only call pv.disposeGUI() and the procMap and scanMap must be
    // freed directly...
    disposables ::= viewMap
    disposables ::= scanMap
    val transport = Transport[S](Mellite.auralSystem) // = proc.Transport [S, workspace.I](group, sampleRate = sampleRate)
    disposables ::= transport
    // val auralView = proc.AuralPresentation.run[S](transport, Mellite.auralSystem, Some(Mellite.sensorSystem))
    // disposables ::= auralView
    transport.addObject(obj)

    // val globalSelectionModel = SelectionModel[S, ProcView[S]]
    val selectionModel  = SelectionModel[S, TimelineObjView[S]]
    val global          = GlobalProcsView(group, selectionModel)
    disposables       ::= global

    val transportView   = TransportView(transport, tlm, hasMillis = true, hasLoop = true)
    val tlView          = new Impl[S](groupH, groupEH, viewMap, scanMap, tlm, selectionModel, global, transportView)

    group.iterator.foreach { case (span, seq) =>
      seq.foreach { timed =>
        tlView.objAdded(span, timed, repaint = false)
      }
    }

    def muteChanged(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val muted   = attr[BooleanElem](ObjKeys.attrMute).exists(_.value)
      tlView.objMuteChanged(timed, muted)
    }

    def nameChanged(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val nameOpt = attr[StringElem](ObjKeys.attrName).map(_.value)
      tlView.objNameChanged(timed, nameOpt)
    }

    def gainChanged(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      val attr  = timed.value.attr
      val gain  = attr[DoubleElem](ObjKeys.attrGain).fold(1.0)(_.value)
      tlView.objGainChanged(timed, gain)
    }

    def busChanged(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val busOpt  = attr[IntElem](ObjKeys.attrBus).map(_.value)
      tlView.procBusChanged(timed, busOpt)
    }

    def fadeChanged(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      val attr    = timed.value.attr
      val fadeIn  = attr[FadeSpec.Elem](ObjKeys.attrFadeIn ).fold(TrackTool.EmptyFade)(_.value)
      val fadeOut = attr[FadeSpec.Elem](ObjKeys.attrFadeOut).fold(TrackTool.EmptyFade)(_.value)
      tlView.objFadeChanged(timed, fadeIn, fadeOut)
    }

    def trackPositionChanged(timed: Timeline.Timed[S])(implicit tx: S#Tx): Unit = {
      val trackIdxNow = timed.value.attr[IntElem](TimelineObjView.attrTrackIndex ).fold(0)(_.value)
      val trackHNow   = timed.value.attr[IntElem](TimelineObjView.attrTrackHeight).fold(4)(_.value)
      tlView.objMoved(timed, spanCh = Change(Span.Void, Span.Void),
        trackCh = Some(trackIdxNow -> trackHNow))
    }

    def attrChanged(timed: Timeline.Timed[S], name: String)(implicit tx: S#Tx): Unit =
      name match {
        case ObjKeys.attrMute  => muteChanged(timed)
        case ObjKeys.attrFadeIn | ObjKeys.attrFadeOut => fadeChanged(timed)
        case ObjKeys.attrName  => nameChanged(timed)
        case ObjKeys.attrGain  => gainChanged(timed)
        case ObjKeys.attrBus   => busChanged (timed)
        case TimelineObjView.attrTrackIndex | TimelineObjView.attrTrackHeight => trackPositionChanged(timed)
        case _ =>
      }

    def scanAdded(timed: TimedProc[S], name: String)(implicit tx: S#Tx): Unit = {
      val proc = timed.value.elem.peer
      proc.scans.get(name).foreach { scan =>
        scan.sources.foreach {
          case Scan.Link.Scan(peer) =>
            tlView.scanSourceAdded(timed, name, scan, peer)
          case _ =>
        }
        scan.sinks.foreach {
          case Scan.Link.Scan(peer) =>
            tlView.scanSinkAdded(timed, name, scan, peer)
          case _ =>
        }
      }
    }

    def scanRemoved(timed: TimedProc[S], name: String)(implicit tx: S#Tx): Unit = {
      val proc = timed.value.elem.peer
      proc.scans.get(name).foreach { scan =>
        scan.sources.foreach {
          case Scan.Link.Scan(peer) =>
            tlView.scanSourceRemoved(timed, name, scan, peer)
          case _ =>
        }
        scan.sinks.foreach {
          case Scan.Link.Scan(peer) =>
            tlView.scanSinkRemoved(timed, name, scan, peer)
          case _ =>
        }
      }
    }

    val obsGroup = group.changed.react { implicit tx => _.changes.foreach {
      case BiGroup.Added  (span, timed) =>
        // println(s"Added   $span, $timed")
        tlView.objAdded(span, timed, repaint = true)

      case BiGroup.Removed(span, timed) =>
        // println(s"Removed $span, $timed")
        tlView.objRemoved(span, timed)

      case BiGroup.ElementMoved  (timed, spanChange) =>
        // println(s"Moved   $timed, $spanCh")
        tlView.objMoved(timed, spanCh = spanChange, trackCh = None)

      case BiGroup.ElementMutated(timed0, procUpd) =>
        if (DEBUG) println(s"Mutated $timed0, $procUpd")
        procUpd.changes.foreach {
          case Obj.ElemChange(updP0) =>
            (timed0.value, updP0) match {
              case (Proc.Obj(procObj), updP1: Proc.Update[_]) =>
                val timed = timed0.asInstanceOf[TimedProc  [S]] // XXX not good
                val updP  = updP1 .asInstanceOf[Proc.Update[S]]
                updP.changes.foreach {
                  case Proc.ScanAdded  (key, _) => scanAdded  (timed, key)
                  case Proc.ScanRemoved(key, _) => scanRemoved(timed, key)
                  case Proc.ScanChange (name, scan, scanUpdates) =>
                    scanUpdates.foreach {
                      case Scan.GraphemeChange(grapheme, segments) =>
                        if (name == Proc.Obj.graphAudio) {
                          // XXX TODO: This doesn't work. Somehow we get a segment that _ends_ at 0L
                          val segmOpt = segments.find(_.span.contains(0L)) match {
                            case Some(segm: Grapheme.Segment.Audio) => Some(segm)
                            case _ => None
                          }
                          tlView.procAudioChanged(timed, segmOpt)
                        }

                      case Scan.SinkAdded(Scan.Link.Scan(peer)) =>
                        val test: Scan[S] = scan
                        tlView.scanSinkAdded(timed, name, test, peer)
                      case Scan.SinkRemoved  (Scan.Link.Scan(peer)) => tlView.scanSinkRemoved  (timed, name, scan, peer)
                      case Scan.SourceAdded  (Scan.Link.Scan(peer)) => tlView.scanSourceAdded  (timed, name, scan, peer)
                      case Scan.SourceRemoved(Scan.Link.Scan(peer)) => tlView.scanSourceRemoved(timed, name, scan, peer)

                      case _ => // Scan.SinkAdded(_) | Scan.SinkRemoved(_) | Scan.SourceAdded(_) | Scan.SourceRemoved(_)
                    }
                  case Proc.GraphChange(_) =>
                }

              case _ =>
            }

          case Obj.AttrAdded  (key, _)    => attrChanged(timed0, key)
          case Obj.AttrRemoved(key, _)    => attrChanged(timed0, key)
          case Obj.AttrChange (key, _, _) => attrChanged(timed0, key)
        }
    }}
    disposables ::= obsGroup

    tlView.disposables.set(disposables)(tx.peer)

    deferTx(tlView.guiInit())
    tlView
  }

  private final class Impl[S <: Sys[S]](groupH            : stm.Source[S#Tx, proc.Timeline[S]],
                                        groupEH           : stm.Source[S#Tx, Timeline.Obj[S]],
                                        val viewMap       : TimelineObjView.Map[S],
                                        val scanMap       : ProcView.ScanMap[S],
                                        val timelineModel : TimelineModel,
                                        val selectionModel: SelectionModel[S, TimelineObjView[S]],
                                        globalView        : GlobalProcsView[S],
                                        transportView     : TransportView[S])
                                       (implicit val workspace: Workspace[S], val cursor: Cursor[S],
                                        val undoManager: UndoManager)
    extends TimelineView[S] with ComponentHolder[Component] with TimelineObjView.Context[S] {
    impl =>

    import cursor.step

    private var viewRange = RangedSeq.empty[TimelineObjView[S], Long]
    private val viewSet   = TSet.empty[TimelineObjView[S]]

    private var canvasView: View    = _
    val disposables           = Ref(List.empty[Disposable[S#Tx]])

    private lazy val toolCursor   = TrackTool.cursor  [S](canvasView)
    private lazy val toolMove     = TrackTool.move    [S](canvasView)
    private lazy val toolResize   = TrackTool.resize  [S](canvasView)
    private lazy val toolGain     = TrackTool.gain    [S](canvasView)
    private lazy val toolMute     = TrackTool.mute    [S](canvasView)
    private lazy val toolFade     = TrackTool.fade    [S](canvasView)
    private lazy val toolFunction = TrackTool.function[S](canvasView)
    private lazy val toolPatch    = TrackTool.patch   [S](canvasView)

    def group     (implicit tx: S#Tx) = groupEH()
    def plainGroup(implicit tx: S#Tx) = groupH()

    def window: Window = component.peer.getClientProperty("de.sciss.mellite.Window").asInstanceOf[Window]

    def canvasComponent: Component = canvasView.canvasComponent

    def dispose()(implicit tx: S#Tx): Unit = {
      deferTx {
        viewRange = RangedSeq.empty
      }
      disposables.swap(Nil)(tx.peer).foreach(_.dispose())
      viewSet.foreach(_.dispose())(tx.peer)
      clearSet(viewSet)
      // these two are already included in `disposables`:
      // viewMap.dispose()
      // scanMap.dispose()
    }

    private def clearSet[A](s: TSet[A])(implicit tx: S#Tx): Unit =
      s.retain(_ => false)(tx.peer) // no `clear` method

    // ---- actions ----

    object actionStopAllSound extends Action("StopAllSound") {
      def apply(): Unit =
        cursor.step { implicit tx =>
          transportView.transport.stop()  // XXX TODO - what else could we do?
          // auralView.stopAll
        }
    }

    object actionBounce extends Action("Bounce") {
      private var settings = ActionBounceTimeline.QuerySettings[S]()

      def apply(): Unit = {
        import ActionBounceTimeline._
        val window  = GUI.findWindow(component)
        val setUpd  = settings.copy(span = timelineModel.selection)
        val (_settings, ok) = query(setUpd, workspace, timelineModel, window = window)
        settings = _settings
        _settings.file match {
          case Some(file) if ok =>
            import Mellite.compiler
            performGUI(workspace, _settings, groupEH, file, window = window)
          case _ =>
        }
      }
    }

    object actionDelete extends Action("Delete") {
      def apply(): Unit = {
        val editOpt = withSelection { implicit tx => views =>
          plainGroup.modifiableOption.flatMap { groupMod =>
            ProcGUIActions.removeProcs(groupMod, views) // XXX TODO - probably should be replaced by Edits.unlinkAndRemove
          }
        }
        editOpt.foreach(undoManager.add)
      }
    }

    object actionSplitObjects extends Action("Split Selected Objects") {
      import KeyStrokes.menu2
      accelerator = Some(menu2 + Key.Y)
      enabled     = false

      def apply(): Unit = {
        val pos     = timelineModel.position
        val pos1    = pos - MinDur
        val pos2    = pos + MinDur
        val editOpt = withFilteredSelection(pv => pv.spanValue.contains(pos1) && pv.spanValue.contains(pos2)) { implicit tx =>
          splitObjects(pos)
        }
        editOpt.foreach(undoManager.add)
      }
    }

    object actionClearSpan extends Action("Clear Selected Span") {
      import KeyStrokes._
      accelerator = Some(menu1 + Key.BackSlash)
      enabled     = false

      def apply(): Unit =
        timelineModel.selection.nonEmptyOption.foreach { selSpan =>
          val editOpt = step { implicit tx =>
            plainGroup.modifiableOption.flatMap { groupMod =>
              editClearSpan(groupMod, selSpan)
            }
          }
          editOpt.foreach(undoManager.add)
        }
    }

    object actionRemoveSpan extends Action("Remove Selected Span") {
      import KeyStrokes._
      accelerator = Some(menu1 + shift + Key.BackSlash)
      enabled = false

      def apply(): Unit = {
        println("TODO: actionRemoveSpan")
        if (true) return
        timelineModel.selection.nonEmptyOption.foreach { selSpan =>
          step { implicit tx =>
            plainGroup.modifiableOption.foreach { groupMod =>
              // ---- remove ----
              // - move everything right of the selection span's stop to the left
              //   by the selection span's length
            }
          }
        }
      }
    }

    // ---- clear ----
    // - find the objects that overlap with the selection span
    // - if the object is contained in the span, remove it
    // - if the object overlaps the span, split it once or twice,
    //   then remove the fragments that are contained in the span
    private def editClearSpan(groupMod: proc.Timeline.Modifiable[S], selSpan: Span)
                             (implicit tx: S#Tx): Option[UndoableEdit] = {
      val allEdits = groupMod.intersect(selSpan).flatMap {
        case (elemSpan, elems) =>
          elems.flatMap { timed =>
            if (selSpan contains elemSpan) {
              Edits.unlinkAndRemove(groupMod, timed.span, timed.value) :: Nil
            } else {
              timed.span match {
                case Expr.Var(oldSpan) =>
                  val (edits1, span2, obj2) = splitObject(groupMod, selSpan.start, oldSpan, timed.value)
                  val edits3 = if (selSpan contains span2.value) edits1 else {
                    val (edits2, _, _) = splitObject(groupMod, selSpan.stop, span2, obj2)
                    edits1 ++ edits2
                  }
                  val edit4 = Edits.unlinkAndRemove(groupMod, span2, obj2)
                  edits3 ++ List(edit4)

                case _ => Nil
              }
            }
          }
      } .toList
      CompoundEdit(allEdits, "Clear Span")
    }

    private def withSelection[A](fun: S#Tx => TraversableOnce[TimelineObjView[S]] => Option[A]): Option[A] =
      if (selectionModel.isEmpty) None else {
       val sel = selectionModel.iterator
        step { implicit tx => fun(tx)(sel) }
      }

    private def withFilteredSelection[A](p: TimelineObjView[S] => Boolean)
                                     (fun: S#Tx => TraversableOnce[TimelineObjView[S]] => Option[A]): Option[A] = {
      val sel = selectionModel.iterator
      val flt = sel.filter(p)
      if (flt.hasNext) step { implicit tx => fun(tx)(flt) } else None
    }

    private def debugCheckConsistency(info: => String)(implicit tx: S#Tx): Unit = if (DEBUG) {
      val check = BiGroupImpl.verifyConsistency(plainGroup, reportOnly = true)
      check.foreach { msg =>
        println(info)
        println(msg)
        sys.error("Rollback")
      }
    }

    def splitObjects(time: Long)(views: TraversableOnce[TimelineObjView[S]])
                    (implicit tx: S#Tx): Option[UndoableEdit] = plainGroup.modifiableOption.flatMap { groupMod =>
      val edits: List[UndoableEdit] = views.flatMap { pv =>
        pv.span() match {
          case Expr.Var(oldSpan) =>
            val (edits, _, _) = splitObject(groupMod, time, oldSpan, pv.obj())
            edits
          case _ => Nil
        }
      } .toList

      CompoundEdit(edits, "Split Objects")
    }

    private def splitObject(groupMod: proc.Timeline.Modifiable[S], time: Long, oldSpan: Expr.Var[S, SpanLike],
                            obj: Obj[S])(implicit tx: S#Tx): (List[UndoableEdit], Expr.Var[S, SpanLike], Obj[S]) = {
      val imp = ExprImplicits[S]
      import imp._
      val leftObj   = obj // pv.obj()
      val rightObj  = ProcActions.copy[S](leftObj /*, Some(oldSpan) */)

      val oldVal    = oldSpan.value
      val rightSpan = oldVal match {
        case Span.HasStart(leftStart) =>
          val _rightSpan  = SpanLikeEx.newVar(oldSpan())
          val resize      = ProcActions.Resize(time - leftStart, 0L)
          val minStart    = timelineModel.bounds.start
          // println("----BEFORE RIGHT----")
          // debugPrintAudioGrapheme(rightObj)
          ProcActions.resize(_rightSpan, rightObj, resize, minStart = minStart)
          // println("----AFTER RIGHT ----")
          // debugPrintAudioGrapheme(rightObj)
          _rightSpan

        case Span.HasStop(rightStop) =>
          SpanLikeEx.newVar(Span(time, rightStop))
      }

      val editLeftSpan: Option[UndoableEdit] = oldVal match {
        case Span.HasStop(rightStop) =>
          val minStart  = timelineModel.bounds.start
          val resize    = ProcActions.Resize(0L, time - rightStop)
          Edits.resize(oldSpan, leftObj, resize, minStart = minStart)

        case Span.HasStart(leftStart) =>
          val leftSpan  = Span(leftStart, time)
          // oldSpan()     = leftSpan
          import SpanLikeEx.{serializer, varSerializer}
          val edit = EditVar.Expr("Resize", oldSpan, leftSpan)
          Some(edit)
      }

      // group.add(rightSpan, rightObj)
      val editAdd = EditTimelineInsertObj("Region", groupMod, rightSpan, rightObj)

      debugCheckConsistency(s"Split left = $leftObj, oldSpan = $oldVal; right = $rightObj, rightSpan = ${rightSpan.value}")
      val list1 = editAdd :: Nil
      val list2 = editLeftSpan.fold(list1)(_ :: list1)
      (list2, rightSpan, rightObj)
    }

    private def debugPrintAudioGrapheme(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      for {
        objT <- Proc.Obj.unapply(obj)
        scan <- objT.elem.peer.scans.get(Proc.Obj.graphAudio)
        Scan.Link.Grapheme(g) <- scan.sources.toList.headOption
      } {
        println(s"GRAPHEME: $g")
        val list = g.debugList()
        println(list)
      }
    }

    def guiInit(): Unit = {
      import desktop.Implicits._

      canvasView = new View
      val ggVisualBoost = new Slider {
        min       = 0
        max       = 64
        value     = 0
        focusable = false
        tooltip   = "Sonogram Brightness"
        peer.putClientProperty("JComponent.sizeVariant", "small")
        listenTo(this)
        reactions += {
          case ValueChanged(_) =>
            import synth._
            canvasView.trackTools.visualBoost = value.linexp(0, 64, 1, 512) // .toFloat
        }
      }
      desktop.Util.fixWidth(ggVisualBoost)

      val actionAttr: Action = Action(null) {
        withSelection { implicit tx =>
          seq => {
            seq.foreach { view =>
              AttrMapFrame(view.obj())
            }
            None
          }
        }
      }

      actionAttr.enabled = false
      val ggAttr = GUI.toolButton(actionAttr, raphael.Shapes.Wrench, "Attributes Editor")
      ggAttr.focusable = false

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          TrackTools.palette(canvasView.trackTools, Vector(
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

      val ggTrackPos = new ScrollBar
      ggTrackPos.maximum = 512 // 128 tracks at default "full-size" (4)
      ggTrackPos.listenTo(ggTrackPos)
      ggTrackPos.reactions += {
        case ValueChanged(_) => canvasView.trackIndexOffset = ggTrackPos.value
      }

      selectionModel.addListener {
        case _ =>
          val hasSome = selectionModel.nonEmpty
          actionAttr        .enabled = hasSome
          actionSplitObjects.enabled = hasSome
      }

      timelineModel.addListener {
        case TimelineModel.Selection(_, span) if span.before.isEmpty != span.now.isEmpty =>
          val hasSome = span.now.nonEmpty
          actionClearSpan .enabled = hasSome
          actionRemoveSpan.enabled = hasSome
      }

      val pane2 = new SplitPane(Orientation.Vertical, globalView.component, canvasView.component)
      pane2.dividerSize = 4
      pane2.border      = null

      val pane = new BorderPanel {
        import BorderPanel.Position._
        layoutManager.setVgap(2)
        add(transportPane, North )
        add(pane2        , Center)
        add(ggTrackPos   , East  )
        // add(globalView.component, West  )
      }

      component = pane
      // DocumentViewHandler.instance.add(this)
    }

    private def repaintAll(): Unit = canvasView.canvasComponent.repaint()

    def objAdded(span: SpanLike, timed: BiGroup.TimedElem[S, Obj[S]], repaint: Boolean)(implicit tx: S#Tx): Unit = {
      logT(s"objAdded($span / ${TimeRef.spanToSecs(span)}, $timed)")
      // timed.span
      // val proc = timed.value

      // val pv = ProcView(timed, viewMap, scanMap)
      val view = TimelineObjView(timed, this)
      viewMap.put(timed.id, view)
      viewSet.add(view)(tx.peer)

      def doAdd(): Unit = view match {
        case pv: ProcView[S] if pv.isGlobal =>
          globalView.add(pv)
        case _ =>
          viewRange += view
          if (repaint) repaintAll()    // XXX TODO: optimize dirty rectangle
        }

      if (repaint)
        deferTx(doAdd())
      else
        doAdd()
    }

    def objRemoved(span: SpanLike, timed: BiGroup.TimedElem[S, Obj[S]])(implicit tx: S#Tx): Unit = {
      logT(s"objRemoved($span, $timed)")
      val id = timed.id
      viewMap.get(id).fold {
        Console.err.println(s"Warning: Timeline - remove object. View for object $timed not found.")
      } { view =>
        viewMap.remove(id)
        viewSet.remove(view)(tx.peer)
        deferTx {
          view match {
            case pv: ProcView[S] if pv.isGlobal => globalView.remove(pv)
            case _ =>
              viewRange -= view
              repaintAll() // XXX TODO: optimize dirty rectangle
          }
        }
        view.dispose()
      }
    }

    // insignificant changes are ignored, therefore one can just move the span without the track
    // by using trackCh = Change(0,0), and vice versa
    def objMoved(timed: BiGroup.TimedElem[S, Obj[S]], spanCh: Change[SpanLike], trackCh: Option[(Int, Int)])
                 (implicit tx: S#Tx): Unit =
      viewMap.get(timed.id).foreach { view =>
        deferTx {
          view match {
            case pv: ProcView[S] if pv.isGlobal => globalView.remove(pv)
            case _                              => viewRange -= view
          }

          if (spanCh .isSignificant) view.spanValue   = spanCh .now
          trackCh.foreach { case (idx, h) =>
            view.trackIndex   = idx
            view.trackHeight  = h
          }

          view match {
            case pv: ProcView[S] if pv.isGlobal => globalView.add(pv)
            case _ =>
              viewRange += view
              repaintAll()  // XXX TODO: optimize dirty rectangle
          }
        }
      }

    private def objUpdated(view: TimelineObjView[S]): Unit = {
      //      if (view.isGlobal)
      //        globalView.updated(view)
      //      else
      repaintAll() // XXX TODO: optimize dirty rectangle
    }

    def objMuteChanged(timed: Timeline.Timed[S], newMute: Boolean)(implicit tx: S#Tx): Unit = {
      val pvo = viewMap.get(timed.id)
      logT(s"objMuteChanged(newMute = $newMute, view = $pvo")
      pvo.foreach {
        case pv: TimelineObjView.HasMute =>
          deferTx {
            pv.muted = newMute
            objUpdated(pv)
          }

        case _ =>
      }
    }

    def objNameChanged(timed: Timeline.Timed[S], newName: Option[String])(implicit tx: S#Tx): Unit = {
      val pvo = viewMap.get(timed.id)
      logT(s"objNameChanged(newName = $newName, view = $pvo")
      pvo.foreach { pv =>
        deferTx {
          pv.nameOption = newName
          objUpdated(pv)
        }
      }
    }

    def procBusChanged(timed: Timeline.Timed[S], newBus: Option[Int])(implicit tx: S#Tx): Unit = {
      val pvo = viewMap.get(timed.id)
      logT(s"procBusChanged(newBus = $newBus, view = $pvo")
      pvo.foreach {
        case pv: ProcView[S] =>
          deferTx {
            pv.busOption = newBus
            objUpdated(pv)
          }

        case _ =>
      }
    }

    def objGainChanged(timed: Timeline.Timed[S], newGain: Double)(implicit tx: S#Tx): Unit = {
      val pvo = viewMap.get(timed.id)
      logT(s"objGainChanged(newGain = $newGain, view = $pvo")
      pvo.foreach {
        case pv: TimelineObjView.HasGain =>
          deferTx {
            pv.gain = newGain
            objUpdated(pv)
          }

        case _ =>
      }
    }

    def objFadeChanged(timed: Timeline.Timed[S], newFadeIn: FadeSpec, newFadeOut: FadeSpec)(implicit tx: S#Tx): Unit = {
      val pvo = viewMap.get(timed.id)
      logT(s"objFadeChanged(newFadeIn = $newFadeIn, newFadeOut = $newFadeOut, view = $pvo")
      pvo.foreach {
        case pv: TimelineObjView.HasFade => deferTx {
          pv.fadeIn   = newFadeIn
          pv.fadeOut  = newFadeOut
          repaintAll()  // XXX TODO: optimize dirty rectangle
        }

        case _ =>
      }
    }

    def procAudioChanged(timed: TimedProc[S], newAudio: Option[Grapheme.Segment.Audio])(implicit tx: S#Tx): Unit = {
      val pvo = viewMap.get(timed.id)
      logT(s"procAudioChanged(newAudio = $newAudio, view = $pvo")
      pvo.foreach {
        case pv: ProcView[S] => deferTx {
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

        case _ =>
      }
    }

    private def withLink(timed: TimedProc[S], that: Scan[S])(fun: (ProcView[S], ProcView[S], String) => Unit)
                        (implicit tx: S#Tx): Unit =
      viewMap.get(timed.id).foreach {
        case thisView: ProcView[S] =>
          scanMap.get(that .id).foreach {
            case (thatKey, thatIdH) =>
              viewMap.get(thatIdH()).foreach {
                case thatView: ProcView[S] =>
                  deferTx {
                    fun(thisView, thatView, thatKey)
                    repaintAll()
                  }
                case _ =>
              }
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

    // TODO - this could be defined by the view?
    // call on EDT!
    private def defaultDropLength(view: ObjView[S], inProgress: Boolean): Long = {
      val d = view match {
        case _: ObjView.AudioGrapheme[S] | _: ObjView.Proc[S] =>
          timelineModel.sampleRate * 2  // two seconds
        case _ =>
          if (inProgress)
            canvasView.screenToFrame(4)   // four pixels
          else
            timelineModel.sampleRate * 1  // one second
      }
      val res = d.toLong
      // println(s"defaultDropLength(inProgress = $inProgress) -> $res"  )
      res
    }

    private def insertAudioRegion(drop: DnD.Drop[S], drag: DnD.AudioDragLike[S],
                                  grapheme: Grapheme.Expr.Audio[S])(implicit tx: S#Tx): Option[UndoableEdit] =
      plainGroup.modifiableOption.map { groupM =>
        logT(s"insertAudioRegion($drop, ${drag.selection}, $grapheme)")
        val tlSpan = Span(drop.frame, drop.frame + drag.selection.length)
        val (span, obj) = ProcActions.mkAudioRegion(time = tlSpan,
          grapheme = grapheme, gOffset = drag.selection.start /*, bus = None */) // , bus = ad.bus.map(_.apply().entity))
        val track = canvasView.screenToTrack(drop.y)
        obj.attr.put(TimelineObjView.attrTrackIndex, Obj(IntElem(IntEx.newVar(IntEx.newConst(track)))))
        val edit = EditTimelineInsertObj("Audio Region", groupM, span, obj)
        edit
      }

    private def performDrop(drop: DnD.Drop[S]): Boolean = {
      def withRegions[A](fun: S#Tx => List[TimelineObjView[S]] => Option[A]): Option[A] =
        canvasView.findRegion(drop.frame, canvasView.screenToTrack(drop.y)).flatMap { hitRegion =>
          val regions = if (selectionModel.contains(hitRegion)) selectionModel.iterator.toList else hitRegion :: Nil
          step { implicit tx =>
            fun(tx)(regions)
          }
        }

      def withProcRegions[A](fun: S#Tx => List[ProcView[S]] => Option[A]): Option[A] =
        canvasView.findRegion(drop.frame, canvasView.screenToTrack(drop.y)).flatMap {
          case hitRegion: ProcView[S] =>
            val regions = if (selectionModel.contains(hitRegion)) {
              selectionModel.iterator.collect {
                case pv: ProcView[S] => pv
              } .toList
            } else hitRegion :: Nil

            step { implicit tx =>
              fun(tx)(regions)
            }
          case _ => None
        }

      // println(s"performDrop($drop)")

      val editOpt: Option[UndoableEdit] = drop.drag match {
        case ad: DnD.AudioDrag[S] =>
          step { implicit tx =>
            insertAudioRegion(drop, ad, ad.source().elem.peer)
          }

        case ed: DnD.ExtAudioRegionDrag[S] =>
          val file = ed.file
          val resOpt = step { implicit tx =>
            val ex = ObjectActions.findAudioFile(workspace.rootH(), file)
            ex.flatMap { grapheme =>
              insertAudioRegion(drop, ed, grapheme.elem.peer)
            }
          }

          resOpt.orElse[UndoableEdit] {
            val tr = Try(AudioFile.readSpec(file)).toOption
            tr.flatMap { spec =>
              ActionArtifactLocation.query[S](workspace.rootH, file).flatMap { either =>
                step { implicit tx =>
                  ActionArtifactLocation.merge(either).flatMap { case (list0, locM) =>
                    val folder  = workspace.rootH()
                    // val obj   = ObjectActions.addAudioFile(elems, elems.size, loc, file, spec)
                    val obj     = ObjectActions.mkAudioFile(locM, file, spec)
                    val edits0  = list0.map(obj => EditFolderInsertObj("Location"  , folder, folder.size, obj)).toList
                    val edits1  = edits0 :+        EditFolderInsertObj("Audio File", folder, folder.size, obj)
                    val edits2  = insertAudioRegion(drop, ed, obj.elem.peer).fold(edits1)(edits1 :+ _)
                    CompoundEdit(edits2, "Insert Audio Region")
                  }
                }
              }
            }
          }

        case DnD.ObjectDrag(_, view: ObjView.Int[S]) => withRegions { implicit tx => regions =>
          val intExpr = view.obj().elem.peer
          Edits.setBus[S](regions.map(_.obj()), intExpr)
        }

        case DnD.ObjectDrag(_, view: ObjView.Code[S]) => withProcRegions { implicit tx => regions =>
          val codeElem = view.obj()
          import Mellite.compiler
          Edits.setSynthGraph[S](regions.map(_.obj()), codeElem)
        }

        case DnD.ObjectDrag(_, view /* : ObjView.Proc[S] */) => step { implicit tx =>
          plainGroup.modifiableOption.map { group =>
            val length  = defaultDropLength(view, inProgress = false)
            val span    = Span(drop.frame, drop.frame + length)
            val spanEx  = SpanLikeEx.newVar[S](SpanLikeEx.newConst(span))
            EditTimelineInsertObj(view.prefix, group, spanEx, view.obj())
          }
          // CompoundEdit(edits, "Insert Objects")
        }

        case pd: DnD.GlobalProcDrag[S] => withProcRegions { implicit tx => regions =>
          val in    = pd.source()
          val edits = regions.flatMap { pv =>
            val out = pv.proc
            Edits.linkOrUnlink[S](out, in)
          }
          CompoundEdit(edits, "Link Global Proc")
        }

        case _ => None
      }

      editOpt.foreach(undoManager.add)
      editOpt.isDefined
    }

    private final class View extends ProcCanvasImpl[S] {
      canvasImpl =>

      private var trkIdxOff = 0

      def trackIndexOffset: Int = trkIdxOff
      def trackIndexOffset_=(value: Int): Unit = {
        trkIdxOff = value
        canvasImpl.repaint()
      }

      // import AbstractTimelineView._
      def timelineModel             = impl.timelineModel
      def selectionModel            = impl.selectionModel
      def timeline(implicit tx: S#Tx)  = impl.plainGroup

      def intersect(span: Span): Iterator[TimelineObjView[S]] = viewRange.filterOverlaps((span.start, span.stop))

      def screenToTrack(y    : Int): Int = y / TrackScale + trkIdxOff
      def trackToScreen(track: Int): Int = (track - trkIdxOff) * TrackScale

      def findRegion(pos: Long, hitTrack: Int): Option[TimelineObjView[S]] = {
        val span      = Span(pos, pos + 1)
        val regions   = intersect(span)
        regions.find(pv => pv.trackIndex <= hitTrack && (pv.trackIndex + pv.trackHeight) > hitTrack)
      }

      protected def commitToolChanges(value: Any): Unit = {
        logT(s"Commit tool changes $value")
        val editOpt = step { implicit tx =>
          value match {
            case t: TrackTool.Cursor    => toolCursor commit t
            case t: TrackTool.Move      =>
              // println("\n----BEFORE----")
              // println(group.debugPrint)
              val res = toolMove.commit(t)
              // println("\n----AFTER----")
              // println(group.debugPrint)
              debugCheckConsistency(s"Move $t")
              res

            case t: TrackTool.Resize    =>
              val res = toolResize commit t
              debugCheckConsistency(s"Resize $t")
              res

            case t: TrackTool.Gain      => toolGain     commit t
            case t: TrackTool.Mute      => toolMute     commit t
            case t: TrackTool.Fade      => toolFade     commit t
            case t: TrackTool.Function  => toolFunction commit t
            case t: TrackTool.Patch[S]  => toolPatch    commit t
            case _ => None
          }
        }
        editOpt.foreach(undoManager.add)
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
        protected def workspace     = impl.workspace

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
          val b = desktop.Util.maximumWindowBounds
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

          val sel = selectionModel

          g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

          val views = viewRange.filterOverlaps((visStart, visStop)).toList // warning: iterator, we need to traverse twice!
          views.foreach { view =>
            val selected  = sel.contains(view)

            def drawProc(start: Long, x1: Int, x2: Int, move: Long): Unit = {
              val pTrk  = if (selected) math.max(0, view.trackIndex + moveState.deltaTrack) else view.trackIndex
              val py    = trackToScreen(pTrk)
              val px    = x1
              val pw    = x2 - x1
              val ph    = trackToScreen(pTrk + view.trackHeight) - py

              // clipped coordinates
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
                view match {
                  case pv: ProcView[S] if pv.audio.isDefined =>
                    val segm        = pv.audio.get
                    val sonogramOpt = pv.sonogram.orElse(pv.acquireSonogram())

                    sonogramOpt.foreach { sonogram =>
                      val audio   = segm.value
                      val srRatio = sonogram.inputSpec.sampleRate / Timeline.SampleRate
                      // dStart is the frame inside the audio-file corresponding
                      // to the region's left margin. That is, if the grapheme segment
                      // starts early than the region (its start is less than zero),
                      // the frame accordingly increases.
                      val dStart  = (audio.offset - segm.span.start +
                                     (if (selected) resizeState.deltaStart else 0L)) * srRatio
                      // a factor to convert from pixel space to audio-file frames
                      val s2f     = timelineModel.visible.length.toDouble / canvasComponent.peer.getWidth * srRatio
                      val lenC    = (px2C - px1C) * s2f
                      val boost   = if (selected) visualBoost * gainState.factor else visualBoost
                      sonogramBoost = (audio.gain * pv.gain).toFloat * boost
                      val startP  = (px1C - px) * s2f + dStart
                      val stopP   = startP + lenC
                      // println(s"${pv.name}; audio.offset = ${audio.offset}, segm.span.start = ${segm.span.start}, dStart = $dStart, px1C = $px1C, startC = $startC, startP = $startP")
                      // println(s"dStart = $dStart, px = $px, px1C = $px1C, startC = $startC, startP = $startP")
                      sonogram.paint(spanStart = startP, spanStop = stopP, g2 = g,
                        tx = px1C, ty = innerY, width = px2C - px1C, height = innerH, ctrl = this)
                    }

                  case _ =>
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

                view match {
                  case fv: TimelineObjView.HasFade if fadeViewMode == FadeViewMode.Curve =>
                    val st      = if (selected) fadeState else NoFade
                    val fdIn    = fv.fadeIn  // XXX TODO: continue here. add delta
                    val fdInFr  = fdIn.numFrames + st.deltaFadeIn
                    if (fdInFr > 0) {
                      val fw    = framesToScreen(fdInFr).toFloat
                      val fdC   = st.deltaFadeInCurve
                      val shape = if (fdC != 0f) adjustFade(fdIn.curve, fdC) else fdIn.curve
                      // if (DEBUG) println(s"fadeIn. fw = $fw, shape = $shape, x = $px")
                      paintFade(shape, fw = fw, y1 = fdIn.floor, y2 = 1f, x = px, x0 = px)
                    }
                    val fdOut   = fv.fadeOut
                    val fdOutFr = fdOut.numFrames + st.deltaFadeOut
                    if (fdOutFr > 0) {
                      val fw    = framesToScreen(fdOutFr).toFloat
                      val fdC   = st.deltaFadeOutCurve
                      val shape = if (fdC != 0f) adjustFade(fdOut.curve, fdC) else fdOut.curve
                      // if (DEBUG) println(s"fadeIn. fw = $fw, shape = $shape, x = ${px + pw - 1 - fw}")
                      val x0    = px + pw - 1
                      paintFade(shape, fw = fw, y1 = 1f, y2 = fdOut.floor, x = x0 - fw, x0 = x0)
                    }

                  case _ =>
                }
                
                g.setClip(clipOrig)
    
                // --- label ---
                if (regionViewMode == RegionViewMode.TitledBox) {
                  val name = view.name
                  // val name = view.name // .orElse(pv.audio.map(_.value.artifact.nameWithoutExtension))
                  g.clipRect(px + 2, py + 2, pw - 4, ph - 4)
                  // possible unicodes: 2327 23DB 24DC 25C7 2715 29BB
                  // val text  = if (view.muted) "\u23DB " + name else name
                  val text: String = view match {
                    case mv: TimelineObjView.HasMute if mv.muted => "\u25C7 " + name
                    case _ => name
                  }
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

                view match {
                  case mv: TimelineObjView.HasMute if mv.muted =>
                    g.setColor(colrRegionBgMuted)
                    g.fillRoundRect(px, py, pw, ph, 5, 5)
                  case _ =>
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

            view.spanValue match {
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

          views.foreach {
            case pv: ProcView[S] =>
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

            case _ =>
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
                drawDropFrame(g, track, 4, span)

              case DnD.ObjectDrag(_, view) /* : ObjView.Proc[S] */ =>
                val track   = screenToTrack(drop.y)
                val length  = defaultDropLength(view, inProgress = true)
                val span    = Span(drop.frame, drop.frame + length)
                drawDropFrame(g, track, 4, span)

              case _ =>
            }
          }

          if (functionState.isValid)
            drawDropFrame(g, functionState.trackIndex, functionState.trackHeight, functionState.span)

          if (patchState.source != null)
            drawPatch(g, patchState)
        }

        private def linkFrame(pv: ProcView[S]): Long = pv.spanValue match {
          case Span(start, stop)  => (start + stop)/2
          case hs: Span.HasStart  => hs.start + (timelineModel.sampleRate * 0.1).toLong
          case _ => 0L
        }

        private def linkY(view: ProcView[S], input: Boolean): Int =
          if (input)
            trackToScreen(view.trackIndex) + 4
          else
            trackToScreen(view.trackIndex + view.trackHeight) - 5

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

        private def drawDropFrame(g: Graphics2D, trackIndex: Int, trackHeight: Int, span: Span): Unit = {
          val x1 = frameToScreen(span.start).toInt
          val x2 = frameToScreen(span.stop ).toInt
          g.setColor(colrDropRegionBg)
          val strkOrig = g.getStroke
          g.setStroke(strkDropRegion)
          val y   = trackToScreen(trackIndex)
          val x1b = math.min(x1 + 1, x2)
          val x2b = math.max(x1b, x2 - 1)
          g.drawRect(x1b, y + 1, x2b - x1b, trackToScreen(trackIndex + trackHeight) - y)
          g.setStroke(strkOrig)
        }
      }
    }
  }
}