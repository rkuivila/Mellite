/*
 *  AuditionImpl.scala
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

import java.awt.event.{MouseAdapter, MouseEvent}
import java.awt.{Cursor, Point, Toolkit}
import javax.swing.undo.UndoableEdit

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{AuralContext, Transport}

object AuditionImpl {
  private lazy val cursor: Cursor = {
    val tk  = Toolkit.getDefaultToolkit
    val img = ToolsImpl.getImage("cursor-audition.png")
    tk.createCustomCursor(img, new Point(4, 4), "Audition")
  }
}

/** The audition tool allows to listen to regions individually.
  * Unfortunately, this is currently quite a hackish solution:
  *
  * - We cannot determine the procs to which selected procs
  *   are linked in terms of scans
  * - We cannot issue correct transport offset play positions,
  *   this implies that the time-reference is undefined and
  *   fades won't work...
  * - We simply add all global procs to make sure that regions
  *   linked up with them play properly.
  *
  * A perhaps better solution would be to have a `AuralTimeline` somehow
  * that smartly filters the objects.
  */
class AuditionImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S], tlv: TimelineView[S])
  extends RegionLike[S, Unit] {

  import TrackTool.{Cursor => _}

  def defaultCursor = AuditionImpl.cursor
  val name          = "Audition"
  val icon          = ToolsImpl.getIcon("audition")

  /** @param e          the event corresponding to the press
    * @param hitTrack   the track index corresponding to the vertical
    *                   mouse coordinate.
    * @param pos        the frame position corresponding to the horizontal
    *                   mouse coordinate
    * @param regionOpt  `Some` timeline object that is beneath the mouse
    *                   position or `None` if the mouse is pressed over
    *                   an empty part of the timeline.
    */
  protected def handlePress(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineObjView[S]]): Unit = {
    handleMouseSelection(e, regionOpt = regionOpt)
    val selMod = canvas.selectionModel
    if (selMod.isEmpty) return

    import tlv.{cursor, workspace}
    val transportOpt = cursor.step { implicit tx =>
      Mellite.auralSystem.serverOption.map { server =>
        implicit val aural = AuralContext(server)
        val transport = Transport[S]
        // transport.position = ...
        transport.play()    // cf. https://github.com/Sciss/SoundProcesses/issues/18
        (tlv.globalView.iterator ++ selMod.iterator).foreach { view =>
          val obj = view.obj()
          transport.addObject(obj)
        }
        transport
      }
    }

    transportOpt.foreach { transport =>
      val c  = e.getComponent
      val ma = new MouseAdapter {
        override def mouseReleased(e: MouseEvent): Unit = {
          cursor.step { implicit tx =>
            transport.dispose()
          }
          c.removeMouseListener(this)
        }
      }
      c.addMouseListener(ma)
    }
  }

  def commit(drag: Unit)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = None
}
