/*
 *  PatchImpl.scala
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

import java.awt.{Color, RenderingHints, Point, Toolkit}
import de.sciss.synth.proc.{Scan, Sys, Proc}
import de.sciss.mellite.gui.TimelineProcCanvas
import de.sciss.mellite.gui.TrackTool
import de.sciss.lucre.expr.Expr
import de.sciss.span.SpanLike
import java.awt.image.BufferedImage
import java.awt.geom.{Ellipse2D, Area}
import javax.swing.ImageIcon
import collection.breakOut
import de.sciss.synth.proc.Scan.Link

object PatchImpl {
  private lazy val image: BufferedImage = {
    val img = new BufferedImage(17, 17, BufferedImage.TYPE_INT_ARGB)
    val g   = img.createGraphics()
    val shp1 =    new Area(new Ellipse2D.Float(0, 0, 17, 17))
    shp1.subtract(new Area(new Ellipse2D.Float(5, 5,  7,  7)))
    val shp2 =    new Area(new Ellipse2D.Float(1, 1, 15, 15))
    shp2.subtract(new Area(new Ellipse2D.Float(4, 4,  9,  9)))
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    g.setColor(Color.white)
    g.fill(shp1)
    g.setColor(Color.black)
    g.fill(shp2)
    g.dispose()
    img
  }

  private lazy val cursor =
    Toolkit.getDefaultToolkit.createCustomCursor(image, new Point(8, 8), "patch")
}
final class PatchImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicRegion[S, TrackTool.Patch[S]] {

  import TrackTool._

  def defaultCursor = PatchImpl.cursor
  val name          = "Patch"
  val icon          = new ImageIcon(PatchImpl.image)

  protected def dragToParam(d: Drag): Patch[S] = {
    val pos   = d.currentPos
    val sink  = canvas.findRegion(frame = pos, hitTrack = d.currentTrack) match {
      case Some(r) if r != d.initial =>
        Patch.Linked(r)
      case _ =>
        Patch.Unlinked(frame = pos, y = d.currentEvent.getY)
    }
    Patch(d.initial, sink)
  }

  private def mkLink(sourceKey: String, source: Scan[S], sinkKey: String, sink: Scan[S])(implicit tx: S#Tx): Unit = {
    log(s"Link $sourceKey to $sinkKey")
    source.addSink(Link.Scan(sink))
  }

  protected def commitProc(drag: Patch[S])(span: Expr[S, SpanLike], out: Proc[S])(implicit tx: S#Tx): Unit =
    drag.sink match {
      case Patch.Linked(view) =>
        val in    = view.procSource()
        val outs0 = out.scans.iterator.toList
        val ins0  = in .scans.iterator.toList
        val outs1: Set[Scan[S]] = outs0.map(_._2)(breakOut)
        val ins1 : Set[Scan[S]] = ins0 .map(_._2)(breakOut)

        // remove scans which are already linked to the other proc
        val outs  = outs0.filterNot { case (key, scan) =>
          scan.sinks  .toList.exists {
            case Link.Scan(peer) if ins1.contains(peer) => true
            case _ => false
          }
        }
        val ins   = ins0 .filterNot { case (key, scan) =>
          scan.sources.toList.exists {
            case Link.Scan(peer) if outs1.contains(peer) => true
            case _ => false
          }
        }

        log(s"Possible outs: ${outs.map(_._1).mkString(", ")}; possible ins: ${ins.map(_._1).mkString(", ")}")

        if (outs.isEmpty || ins.isEmpty) return   // nothing to patch
        if (outs.size == 1 && ins.size == 1) {    // exactly one possible connection, go ahead
          val (sourceKey, source) = outs.head
          val (sinkKey  , sink  ) = ins .head
          mkLink(sourceKey, source, sinkKey, sink)

        } else {  // present dialog to user
          println(s"Woop. Multiple choice... Dialog not yet implemented...")
        }

      case _ =>
    }

  protected def dialog(): Option[Patch[S]] = {
    println("Not yet implemented - movement dialog")
    None
  }
}
