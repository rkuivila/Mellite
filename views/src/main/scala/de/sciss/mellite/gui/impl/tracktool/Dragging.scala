/*
 *  Dragging.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
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
package tracktool

import java.awt.event.{KeyListener, MouseEvent, KeyEvent}
import javax.swing.event.MouseInputAdapter
import de.sciss.mellite.gui.TrackTool.{DragAdjust, DragBegin, DragEnd, DragCancel}
import de.sciss.lucre.synth.Sys

trait Dragging[S <: Sys[S], A] {
  _: RegionLike[S, A] =>

  protected def dragToParam(d: Drag): A

  protected type Initial

  final protected var currentParam = Option.empty[A]

  final protected def dragEnd   ()       : Unit = dispatch(DragEnd   )
  final protected def dragCancel(d: Drag): Unit = dispatch(DragCancel)

  /* final */ protected def dragStarted(d: this.Drag): Boolean =
    d.currentEvent.getPoint.distanceSq(d.firstEvent.getPoint) > 16

  final protected def dragBegin(d: Drag): Unit = {
    val p = dragToParam(d)
    currentParam = Some(p)
    dispatch(DragBegin)
    dispatch(DragAdjust(p))
  }

  final protected def dragAdjust(d: Drag): Unit =
    currentParam.foreach { oldP =>
      val p = dragToParam(d)
      if (p != oldP) {
        currentParam = Some(p)
        dispatch(DragAdjust(p))
      }
    }

  protected class Drag(val firstEvent: MouseEvent, val firstTrack: Int,
                       val firstPos: Long, val initial: Initial)
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
      comp.requestFocus() // (why? needed to receive key events?)
    }

    override def mouseReleased(e: MouseEvent): Unit = {
      unregister()
      if (started) dragEnd()
    }

    private def unregister(): Unit = {
      val comp = firstEvent.getComponent
      comp.removeMouseListener      (this)
      comp.removeMouseMotionListener(this)
      comp.removeKeyListener        (this)
    }

    private def calcCurrent(e: MouseEvent): Unit = {
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

    override def mouseDragged(e: MouseEvent): Unit = {
      calcCurrent(e)
      if (!started) {
        started = dragStarted(this)
        if (!started) return
        e.getComponent.addKeyListener(this)
        dragBegin(this)
      }
      dragAdjust(this)
    }

    def keyPressed(e: KeyEvent): Unit =
      if (e.getKeyCode == KeyEvent.VK_ESCAPE) {
        unregister()
        dragCancel(this)
      }

    def keyTyped   (e: KeyEvent) = ()
    def keyReleased(e: KeyEvent) = ()
  }
}