/*
 *  DnD.scala
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

import javax.swing.TransferHandler._
import java.awt.dnd.{DropTarget, DropTargetDropEvent, DropTargetEvent, DropTargetDragEvent, DropTargetAdapter}
import de.sciss.synth.proc.Sys
import scala.swing.Component
import java.awt.Point
import de.sciss.lucre.stm
import de.sciss.mellite.Element.AudioGrapheme
import de.sciss.span.Span
import de.sciss.audiowidgets.TimelineModel
import de.sciss.mellite.Document

object DnD {
  sealed trait Drag[S <: Sys[S]] {
    def document: Document[S]
    def source: stm.Source[S#Tx, Element[S]]
  }
  final case class AudioDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, AudioGrapheme[S]],
                                          /* grapheme: Grapheme.Value.Audio, */ selection: Span,
                                          bus: Option[stm.Source[S#Tx, Element.Int[S]]])
    extends Drag[S]

  final case class IntDrag [S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Element.Int [S]]) extends Drag[S]
  final case class CodeDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Element.Code[S]]) extends Drag[S]

  final case class Drop[S <: Sys[S]](frame: Long, y: Int, drag: Drag[S])

  final val flavor = DragAndDrop.internalFlavor[Drag[_]]
}
trait DnD[S <: Sys[S]] {
  _: Component =>

  import DnD._

  protected def document: Document[S]
  protected def timelineModel: TimelineModel

  protected def updateDnD(drop: Option[Drop[S]]): Unit
  protected def acceptDnD(drop:        Drop[S] ): Boolean

  private object Adaptor extends DropTargetAdapter {
    override def dragEnter(e: DropTargetDragEvent): Unit = process(e)
    override def dragOver (e: DropTargetDragEvent): Unit = process(e)
    override def dragExit (e: DropTargetEvent    ): Unit = updateDnD(None)

    private def abortDrag(e: DropTargetDragEvent): Unit = {
      updateDnD(None)
      e.rejectDrag()
    }

    private def mkDrop(d: DnD.Drag[S], loc: Point): Drop[S] = {
      val visi  = timelineModel.visible
      val w     = peer.getWidth
      val frame = (loc.x.toDouble / w * visi.length + visi.start).toLong
      val y     = loc.y
      Drop(frame = frame, y = y, drag = d)
    }

    private def process(e: DropTargetDragEvent): Unit = {
      val t = e.getTransferable
      if (!t.isDataFlavorSupported(DnD.flavor)) {
        abortDrag(e)

      } else t.getTransferData(DnD.flavor) match {
        case d: DnD.Drag[_] if d.document == document =>
          val loc     = e.getLocation
          val drag    = d.asInstanceOf[DnD.Drag[S]]
          val drop    = mkDrop(drag, loc)
          updateDnD(Some(drop))
          e.acceptDrag(COPY)

        case _ =>
          abortDrag(e)
      }
    }

    def drop(e: DropTargetDropEvent): Unit = {
      updateDnD(None)

      val t = e.getTransferable
      if (!t.isDataFlavorSupported(DnD.flavor)) {
        e.rejectDrop()

      } else t.getTransferData(DnD.flavor) match {
        case d: DnD.Drag[_] =>
          val drag    = d.asInstanceOf[DnD.Drag[S]]
          e.acceptDrop(COPY)
          val loc     = e.getLocation
          val drop    = mkDrop(drag, loc)
          val success = acceptDnD(drop)
          e.dropComplete(success)

        case _ =>
          e.rejectDrop()
      }
    }
  }

  new DropTarget(peer, COPY, Adaptor)
}
