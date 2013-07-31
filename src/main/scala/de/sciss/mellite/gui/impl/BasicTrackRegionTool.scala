package de.sciss
package mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import java.awt.event.{KeyEvent, KeyListener, MouseEvent}
import javax.swing.event.MouseInputAdapter

object BasicTrackRegionTool {
  final val MinDur  = 32
}
trait BasicTrackRegionTool[S <: Sys[S], A] extends TrackRegionToolImpl[S, A] {
  import TrackTool._

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

  /* final */ protected def dragStarted(d: this.Drag): Boolean =
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
      _currentPos   = canvas.screenToFrame(e.getX).toLong
      _currentTrack = canvas.screenToTrack(e.getY - firstEvent.getY) + canvas.screenToTrack(firstEvent.getY)
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


