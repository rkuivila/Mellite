/*
 *  BasicRegion.scala
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
package tracktool

import de.sciss.synth.proc.Sys
import java.awt.event.{KeyEvent, KeyListener, MouseEvent}
import javax.swing.event.MouseInputAdapter

object BasicRegion {
  final val MinDur  = 32
}
trait BasicRegion[S <: Sys[S], A] extends RegionImpl[S, A] {
  import TrackTool._

  final protected var _currentParam = Option.empty[A]

  protected def dragToParam(d: Drag): A

  final protected def dragEnd   ()       : Unit = dispatch(DragEnd   )
  final protected def dragCancel(d: Drag): Unit = dispatch(DragCancel)

  final protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: timeline.ProcView[S]): Unit =
    if (e.getClickCount == 2) {
      handleDoubleClick()
    } else {
      new Drag(e, hitTrack, pos, region)
    }

  /* final */ protected def dragStarted(d: this.Drag): Boolean =
    d.currentEvent.getPoint.distanceSq(d.firstEvent.getPoint) > 16

  final protected def dragBegin(d: Drag): Unit = {
    val p = dragToParam(d)
    _currentParam = Some(p)
    dispatch(DragBegin)
    dispatch(DragAdjust(p))
  }

  final protected def dragAdjust(d: Drag): Unit =
    _currentParam.foreach { oldP =>
      val p = dragToParam(d)
      if (p != oldP) {
        _currentParam = Some(p)
        dispatch(DragAdjust(p))
      }
    }

  protected def dialog(): Option[A]

  final protected def handleDoubleClick(): Unit =
    dialog().foreach { p =>
      dispatch(DragBegin)
      dispatch(DragAdjust(p))
      dispatch(DragEnd)
    }

  //  protected def showDialog(message: AnyRef): Boolean = {
  //    val op = OptionPane(message = message, messageType = OptionPane.Message.Question,
  //      optionType = OptionPane.Options.OkCancel)
  //    val result = Window.showDialog(op -> name)
  //    result == OptionPane.Result.Ok
  //  }

  protected class Drag(val firstEvent: MouseEvent, val firstTrack: Int,
                       val firstPos: Long, val firstRegion: timeline.ProcView[S])
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


