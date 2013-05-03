package de.sciss
package mellite
package gui

import java.awt.Cursor
import javax.swing.event.MouseInputAdapter
import java.awt.event.{KeyEvent, KeyListener, MouseEvent}
import annotation.switch
import model.Model
import de.sciss.mellite.gui.impl.{TrackToolsImpl, TimelineProcView}
import de.sciss.lucre.event.Change
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.Sys

object TrackTools {
  sealed trait Update[S <: Sys[S]]
  final case class ToolChanged[S <: Sys[S]]          (change: Change[TrackTool[S]  ]) extends Update[S]
  final case class VisualBoostChanged[S <: Sys[S]]   (change: Change[Float         ]) extends Update[S]
  final case class FadeViewModeChanged[S <: Sys[S]]  (change: Change[FadeViewMode  ]) extends Update[S]
  final case class RegionViewModeChanged[S <: Sys[S]](change: Change[RegionViewMode]) extends Update[S]

  def apply[S <: Sys[S]](timelineModel: TimelineModel): TrackTools[S] = new TrackToolsImpl[S](timelineModel)
}

object RegionViewMode {
  /** No visual indicator for region borders */
  case object None extends RegionViewMode { final val id = 0 }
  /** Rounded box for region borders */
  case object Box extends RegionViewMode { final val id = 1 }
  /** Rounded box with region name for region borders */
  case object TitledBox extends RegionViewMode { final val id = 2 }

  def apply(id: Int): RegionViewMode = (id: @switch) match {
    case None     .id => None
    case Box      .id => Box
    case TitledBox.id => TitledBox
  }
}
sealed trait RegionViewMode { def id: Int }

object FadeViewMode {
  /** No visual indicator for fades */
  case object None extends FadeViewMode { final val id = 0 }
  /** Curve overlays to indicate fades */
  case object Curve extends FadeViewMode { final val id = 1 }
  /** Gain adjustments to sonogram to indicate fades */
  case object Sonogram extends FadeViewMode { final val id = 2 }

  def apply(id: Int): FadeViewMode = (id: @switch) match {
    case None    .id  => None
    case Curve   .id  => Curve
    case Sonogram.id  => Sonogram
  }
}
sealed trait FadeViewMode {
  def id: Int
}

trait TrackTools[S <: Sys[S]] extends Model[TrackTools.Update[S]] {
  var currentTool: TrackTool[S]
  var visualBoost: Float
  var fadeViewMode: FadeViewMode
  var regionViewMode: RegionViewMode
}

trait TrackTool[S <: Sys[S]] /* extends Model */ {
  def defaultCursor: Cursor
  def name: String
  def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineProcView[S]]): Unit
}

//trait TrackToolsListener {
//  def registerTools(tools: TrackTools)
//}

final class TrackCursorTool[S <: Sys[S]](timelineModel: TimelineModel)
  extends TrackTool[S] {

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR)
  val name          = "Cursor"

  def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineProcView[S]]) {}
}

object TrackRegionTool {
  sealed trait Update[+A]
  case object DragBegin  extends Update[Nothing]
  final case class DragAdjust[A](value: A) extends Update[A]
  case object DragEnd    extends Update[Nothing] // (commit: AbstractCompoundEdit)
  case object DragCancel extends Update[Nothing]
}

trait TrackRegionTool[S <: Sys[S], A] extends TrackTool[S] with ModelImpl[TrackRegionTool.Update[A]] {
  tool =>

  // protected def trackList: TrackList
  protected def timelineModel: TimelineModel
  protected def selectionModel: ProcSelectionModel[S]

  def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineProcView[S]]) {
    if (e.isShiftDown) {
      regionOpt.foreach { region =>
        if (selectionModel.contains(region)) {
          selectionModel -= region
        } else {
          selectionModel += region
        }
      }
    } else {
      if (regionOpt.map(region => !selectionModel.contains(region)) getOrElse true) {
        // either hitten a region which wasn't selected, or hitting an empty area
        // --> deselect all
        selectionModel.clear()
        regionOpt.foreach(selectionModel += _)
      }
    }

    // now go on if region is selected
    regionOpt.foreach(region => if (selectionModel.contains(region)) {
      handleSelect(e, hitTrack, pos, region)
    })
  }

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView[S]): Unit

  protected def screenToVirtual(e: MouseEvent): Long = {
    val tlSpan = timelineModel.visible // bounds
    val p_off = -tlSpan.start
    val p_scale = e.getComponent.getWidth.toDouble / tlSpan.length
    (e.getX.toLong / p_scale - p_off + 0.5).toLong
  }
}

trait BasicTrackRegionTool[S <: Sys[S], A] extends TrackRegionTool[S, A] {

  import TrackRegionTool._

  final protected var _currentParam = Option.empty[A]

  protected def dragToParam(d: Drag): A

  final protected def dragEnd() {
    dispatch(DragEnd)
  }

  final protected def dragCancel(d: Drag) {
    dispatch(DragCancel)
  }

  final protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView[S]) {
    if (e.getClickCount == 2) {
      handleDoubleClick()
    } else {
      new Drag(e, hitTrack, pos, region)
    }
  }

  final protected def dragStarted(d: this.Drag): Boolean =
    d.currentEvent.getPoint.distanceSq(d.firstEvent.getPoint) > 16

  final protected def dragBegin(d: Drag) {
    val p = dragToParam(d)
    _currentParam = Some(p)
    dispatch(DragBegin)
    dispatch(DragAdjust(p))
  }

  final protected def dragAdjust(d: Drag) {
    _currentParam.foreach { oldP =>
      val p = dragToParam(d)
      if (p != oldP) {
        _currentParam = Some(p)
        dispatch(DragAdjust(p))
      }
    }
  }

  protected def dialog(): Option[A]

  final protected def handleDoubleClick() {
    dialog().foreach { p =>
      dispatch(DragBegin)
      dispatch(DragAdjust(p))
      dispatch(DragEnd)
    }
  }

  //  protected def showDialog(message: AnyRef): Boolean = {
  //    val op = OptionPane(message = message, messageType = OptionPane.Message.Question,
  //      optionType = OptionPane.Options.OkCancel)
  //    val result = Window.showDialog(op -> name)
  //    result == OptionPane.Result.Ok
  //  }

  protected class Drag(val firstEvent: MouseEvent, val firstTrack: Int,
                       val firstPos: Long, val firstRegion: TimelineProcView[S])
    extends MouseInputAdapter with KeyListener {

    private var started         = false
    private var _currentEvent   = firstEvent
    private var _currentTrack   = firstTrack
    private var _currentPos     = firstPos

    def currentEvent  = _currentEvent
    def currentTrack  = _currentTrack
    def currentPos    = _currentPos

    // ---- constructor ----
    {
      val comp = firstEvent.getComponent
      comp.addMouseListener(this)
      comp.addMouseMotionListener(this)
      //       comp.addKeyListener( this )
      comp.requestFocus() // (why? needed to receive key events?)
    }

    override def mouseReleased(e: MouseEvent) {
      unregister()
      if (started) dragEnd()
    }

    private def unregister() {
      val comp = firstEvent.getComponent
      comp.removeMouseListener      (this)
      comp.removeMouseMotionListener(this)
      comp.removeKeyListener        (this)
    }

    private def calcCurrent(e: MouseEvent) {
      _currentEvent = e
      //      _currentTrack = firstTrack // default assumption
      //      val comp = e.getComponent
      //      if (e.getX < 0 || e.getX >= comp.getWidth ||
      //          e.getY < 0 || e.getY >= comp.getHeight) {
      //
      //        val parent    = comp.getParent
      //        val ptParent  = SwingUtilities.convertPoint(comp, e.getX, e.getY, parent)
      //        val child     = parent.getComponentAt(ptParent)
      //        if (child != null) {
      //          _currentTrack = trackList.find(_.renderer.trackComponent == child).getOrElse(firstTrack)
      //        }
      //      }
      //      val convE     = SwingUtilities.convertMouseEvent(comp, e, _currentTrack.renderer.trackComponent)
      _currentPos   = screenToVirtual(e)
      _currentTrack = (e.getY - firstEvent.getY) / 32 + firstEvent.getY / 32
    }

    override def mouseDragged(e: MouseEvent) {
      calcCurrent(e)
      if (!started) {
        started = dragStarted(this)
        if (!started) return
        e.getComponent.addKeyListener(this)
        dragBegin(this)
      }
      dragAdjust(this)
    }

    def keyPressed(e: KeyEvent) {
      if (e.getKeyCode == KeyEvent.VK_ESCAPE) {
        unregister()
        dragCancel(this)
      }
    }

    def keyTyped   (e: KeyEvent) {}
    def keyReleased(e: KeyEvent) {}
  }
}

object TrackMoveTool {
  final case class Move(deltaTime: Long, deltaTrack: Int, copy: Boolean)
}
final class TrackMoveTool[S <: Sys[S]](protected val timelineModel: TimelineModel,
                                       protected val selectionModel: ProcSelectionModel[S])
  extends BasicTrackRegionTool[S, TrackMoveTool.Move] {

  import TrackMoveTool._

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  val name          = "Move"

  protected def dragToParam(d: Drag): Move = {
    Move(deltaTime = d.currentPos - d.firstPos, deltaTrack = d.currentTrack - d.firstTrack,
      copy = d.currentEvent.isAltDown)
  }

  protected def dialog(): Option[Move] = {
    //    val box             = Box.createHorizontalBox
    //    val timeTrans       = new DefaultUnitTranslator()
    //    val ggTime          = new BasicParamField(timeTrans)
    //    val spcTimeHHMMSSD  = new ParamSpace(Double.NegativeInfinity, Double.PositiveInfinity, 0.0, 1, 3, 0.0,
    //      ParamSpace.TIME | ParamSpace.SECS | ParamSpace.HHMMSS | ParamSpace.OFF)
    //    ggTime.addSpace(spcTimeHHMMSSD)
    //    ggTime.addSpace(ParamSpace.spcTimeSmpsD)
    //    ggTime.addSpace(ParamSpace.spcTimeMillisD)
    //    GUIUtil.setInitialDialogFocus(ggTime)
    //    box.add(new JLabel("Move by:"))
    //    box.add(Box.createHorizontalStrut(8))
    //    box.add(ggTime)
    //
    //    val tl = timelineModel.timeline
    //    timeTrans.setLengthAndRate(tl.span.length, tl.rate)
    //    if (showDialog(box)) {
    //      val delta = timeTrans.translate(ggTime.value, ParamSpace.spcTimeSmpsD).value.toLong
    //      Some(Move(delta, 0, copy = false))
    //    } else 
    None
  }
}

//object TrackResizeTool {
//  case class Resize(deltaStart: Long, deltaStop: Long)
//}
//class TrackResizeTool(trackList: TrackList, timelineModel: TimelineView)
//  extends BasicTrackRegionTool[TrackResizeTool.Resize](trackList, timelineModel) {
//
//  import TrackResizeTool._
//
//  def defaultCursor = Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)
//
//  val name = "Resize"
//
//  protected def dialog: Option[Resize] = None // not yet supported
//
//  protected def dragToParam(d: Drag): Resize = {
//    val (deltaStart, deltaStop) =
//      if (math.abs(d.firstPos - d.firstRegion.span.start) <
//        math.abs(d.firstPos - d.firstRegion.span.stop)) {
//
//        (d.currentPos - d.firstPos, 0L)
//      } else {
//        (0L, d.currentPos - d.firstPos)
//      }
//    Resize(deltaStart, deltaStop)
//  }
//}

//object TrackGainTool {
//  case class Gain(factor: Float)
//}
//class TrackGainTool(trackList: TrackList, timelineModel: TimelineView)
//  extends BasicTrackRegionTool[TrackGainTool.Gain](trackList, timelineModel) {
//
//  import TrackGainTool._
//
//  def defaultCursor = Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)
//
//  val name = "Gain"
//
//  protected def dialog: Option[Gain] = None // not yet supported
//
//  override protected def dragStarted(d: this.Drag): Boolean =
//    d.currentEvent.getY != d.firstEvent.getY
//
//  protected def dragToParam(d: Drag): Gain = {
//    val dy = d.firstEvent.getY - d.currentEvent.getY
//    // use 0.1 dB per pixel. eventually we could use modifier keys...
//    val factor = (dbamp(dy / 10)).toFloat
//    Gain(factor)
//  }
//}

//object TrackFadeTool {
//  case class Fade(deltaFadeIn: Long, deltaFadeOut: Long,
//                  deltaFadeInCurve: Float, deltaFadeOutCurve: Float)
//}
//
//class TrackFadeTool(trackList: TrackList, timelineModel: TimelineView)
//  extends BasicTrackRegionTool[TrackFadeTool.Fade](trackList, timelineModel) {
//
//  import TrackFadeTool._
//
//  def defaultCursor = Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR)
//
//  val name = "Fade"
//
//  private var curvature = false
//
//  protected def dialog: Option[Fade] = None // not yet supported
//
//  protected def dragToParam(d: Drag): Fade = {
//    val leftHand = math.abs(d.firstPos - d.firstRegion.span.start) <
//      math.abs(d.firstPos - d.firstRegion.span.stop)
//    val (deltaTime, deltaCurve) = if (curvature) {
//      val dc = (d.firstEvent.getY - d.currentEvent.getY) * 0.1f
//      (0L, if (leftHand) -dc else dc)
//    } else {
//      (if (leftHand) d.currentPos - d.firstPos else d.firstPos - d.currentPos, 0f)
//    }
//    if (leftHand) Fade(deltaTime, 0L, deltaCurve, 0f)
//    else Fade(0L, deltaTime, 0f, deltaCurve)
//  }
//
//  override protected def dragStarted(d: this.Drag): Boolean = {
//    val result = super.dragStarted(d)
//    if (result) {
//      curvature = math.abs(d.currentEvent.getX - d.firstEvent.getX) <
//        math.abs(d.currentEvent.getY - d.firstEvent.getY)
//    }
//    result
//  }
//}

//object TrackSlideTool {
//  case class Slide(deltaOuter: Long, deltaInner: Long)
//}
//
//class TrackSlideTool(trackList: TrackList, timelineModel: TimelineView)
//  extends BasicTrackRegionTool[TrackSlideTool.Slide](trackList, timelineModel) {
//
//  import TrackSlideTool._
//
//  def defaultCursor = Cursor.getPredefinedCursor(Cursor.E_RESIZE_CURSOR)
//
//  val name = "Slide"
//
//  protected def dialog: Option[Slide] = None // not yet supported
//
//  protected def dragToParam(d: Drag): Slide = {
//    val amt = d.currentPos - d.firstPos
//    if (d.firstEvent.isAltDown)
//      Slide(0L, -amt)
//    else
//      Slide(amt, 0L)
//  }
//}

//object TrackMuteTool {
//  case class Mute(ce: AbstractCompoundEdit, state: Boolean)
//
//  private lazy val cursor = {
//    val tk = Toolkit.getDefaultToolkit
//    val img = tk.createImage(classOf[TrackMuteTool].getResource("cursor-mute.png"))
//    tk.createCustomCursor(img, new Point(4, 4), "Mute")
//  }
//}
//
//class TrackMuteTool(protected val trackList: TrackList, protected val timelineModel: TimelineView)
//  extends TrackRegionTool {
//
//  import TrackMuteTool._
//
//  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView) {
//    region match {
//      case mStake: MuteableTimelineProcView =>
//        timelineModel.timeline.editor.foreach { ed =>
//          val ce = ed.editBegin(name)
//          dispatch(Mute(ce, !mStake.muted))
//          ed.editEnd(ce)
//        }
//
//      case _ =>
//    }
//  }
//
//  val defaultCursor = TrackMuteTool.cursor
//  val name = "Mute"
//}

//object TrackAuditionTool {
//  private lazy val cursor = {
//    val tk  = Toolkit.getDefaultToolkit
//    val img = tk.createImage(classOf[TrackMuteTool].getResource("cursor-audition.png"))
//    tk.createCustomCursor(img, new Point(4, 4), "Audition")
//  }
//}
//
//class TrackAuditionTool(doc: Session, protected val trackList: TrackList, protected val timelineModel: TimelineView)
//  extends TrackRegionTool {
//
//  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView) {
//    val fromStart = e.isAltDown
//    if (!fromStart) {
//      timelineModel.editor.foreach { ed =>
//        val ce = ed.editBegin("Adjust timeline position")
//        ed.editPosition(ce, pos)
//        ed.editEnd(ce)
//      }
//    }
//    val trackPlayerO = SuperColliderClient.instance.getPlayer(doc).flatMap(_.session)
//      .map(_.timeline(timelineModel.timeline).track(tle.track))
//    (region, trackPlayerO) match {
//      case (ar: AudioRegion, Some(atp: SCAudioTrackPlayer)) =>
//        val frameOffset = if (fromStart) 0L
//        else {
//          ar.span.clip(pos - (ar.audioFile.sampleRate * 0.1).toLong) - ar.span.start
//        }
//        val stopper = atp.play(ar, frameOffset)
//        val comp = e.getComponent
//        comp.addMouseListener(new MouseAdapter {
//          override def mouseReleased(e2: MouseEvent) {
//            stopper.stop()
//            comp.removeMouseListener(this)
//          }
//        })
//        comp.requestFocus()
//
//      case _ =>
//    }
//  }
//
//  val name          = "Audition"
//  val defaultCursor = TrackAuditionTool.cursor
//}