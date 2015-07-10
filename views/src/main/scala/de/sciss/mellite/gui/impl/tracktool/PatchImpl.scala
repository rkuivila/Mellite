/*
 *  PatchImpl.scala
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

import java.awt.event.MouseEvent
import java.awt.geom.{Area, Ellipse2D}
import java.awt.image.BufferedImage
import java.awt.{Color, Point, RenderingHints, Toolkit}
import javax.swing.ImageIcon
import javax.swing.undo.UndoableEdit

import de.sciss.desktop.Desktop
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.Edits
import de.sciss.span.SpanLike
import de.sciss.swingplus.PaddedIcon
import de.sciss.synth.proc.{Obj, Proc}

import scala.swing.Insets

object PatchImpl {
  private def mkImage(aa: Boolean): BufferedImage = {
    val img   = new BufferedImage(17, 17, BufferedImage.TYPE_INT_ARGB)
    val g     = img.createGraphics()
    val shp1  =   new Area(new Ellipse2D.Float(0, 0, 17, 17))
    shp1 subtract new Area(new Ellipse2D.Float(5, 5,  7,  7))
    val shp2  =   new Area(new Ellipse2D.Float(1, 1, 15, 15))
    shp2 subtract new Area(new Ellipse2D.Float(4, 4,  9,  9))
    if (aa) g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(Color.white)
    g.fill(shp1)
    g.setColor(Color.black)
    g.fill(shp2)
    g.dispose()
    img
  }

  lazy val image: BufferedImage = mkImage(aa = true)

  // anti-aliased transparency seem unsupported on Linux
  private lazy val cursor =
    Toolkit.getDefaultToolkit.createCustomCursor(mkImage(aa = Desktop.isMac), new Point(8, 8), "patch")
}
final class PatchImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends RegionImpl[S, TrackTool.Patch[S]] with Dragging[S, TrackTool.Patch[S]] {

  import TrackTool._

  def defaultCursor = PatchImpl.cursor
  val name          = "Patch"
  val icon          = new PaddedIcon(new ImageIcon(PatchImpl.image), new Insets(1, 1, 2, 2)) // make it 20x20

  protected type Initial = ProcObjView.Timeline[S] // TimelineObjView[S]

  protected def dragToParam(d: Drag): Patch[S] = {
    val pos   = d.currentPos
    val sink  = canvas.findRegion(frame = pos, hitTrack = d.currentTrack) match {
      case Some(r: ProcObjView.Timeline[S]) if r != d.initial /* && r.inputs.nonEmpty */ =>  // region.inputs only carries linked ones!
        Patch.Linked(r)
      case _ =>
        Patch.Unlinked(frame = pos, y = d.currentEvent.getY)
    }
    Patch(d.initial, sink)
  }

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineObjView[S]): Unit =
    region match {
      case pv: ProcObjView.Timeline[S] => new Drag(e, hitTrack, pos, pv) // region.outputs only carries linked ones!
      case _ =>
    }

  protected def commitObj(drag: Patch[S])(span: Expr[S, SpanLike], outObj: Obj[S])
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] =
    (drag.sink, outObj) match {
      case (Patch.Linked(view), Proc.Obj(out)) =>
        val in = view.obj
        Edits.linkOrUnlink(out, in)

      case _ => None
    }
}
