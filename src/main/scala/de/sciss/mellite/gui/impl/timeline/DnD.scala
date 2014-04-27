/*
 *  DnD.scala
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

import javax.swing.TransferHandler._
import java.awt.dnd.{DropTarget, DropTargetDropEvent, DropTargetEvent, DropTargetDragEvent, DropTargetAdapter}
import de.sciss.synth.proc.{ProcElem, Obj, AudioGraphemeElem, IntElem}
import scala.swing.Component
import java.awt.Point
import de.sciss.lucre.stm
import de.sciss.span.Span
import de.sciss.audiowidgets.TimelineModel
import de.sciss.mellite.Document
import java.awt.datatransfer.{DataFlavor, Transferable}
import de.sciss.file._
import scala.util.Try
import de.sciss.lucre.event.Sys

object DnD {
  sealed trait Drag[S <: Sys[S]] {
    def document: Document[S]
    // def source: stm.Source[S#Tx, Element[S]]
  }
  sealed trait AudioDragLike[S <: Sys[S]] extends Drag[S] {
    def selection: Span
  }
  final case class AudioDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]],
                                          /* grapheme: Grapheme.Value.Audio, */ selection: Span,
                                          bus: Option[stm.Source[S#Tx, Obj.T[S, IntElem]]])
    extends AudioDragLike[S]

  final case class IntDrag [S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Obj.T[S, IntElem  ]]) extends Drag[S]
  final case class CodeDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Obj.T[S, Code.Elem]]) extends Drag[S]
  final case class ProcDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Obj.T[S, ProcElem ]]) extends Drag[S]

  /** Drag and Drop from Eisenkraut */
  final case class ExtAudioRegionDrag[S <: Sys[S]](document: Document[S], file: File, selection: Span)
    extends AudioDragLike[S]

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

    private def abortDrag (e: DropTargetDragEvent): Unit = {
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

    private def mkExtStringDrag(t: Transferable, isDragging: Boolean): Option[DnD.ExtAudioRegionDrag[S]] = {
      // stupid OS X doesn't give out the string data before drop actually happens
      if (isDragging) {
        return Some(ExtAudioRegionDrag(document, file(""), Span(0, 0)))
      }

      val data  = t.getTransferData(DataFlavor.stringFlavor)
      val str   = data.toString
      val arr   = str.split(":")
      if (arr.length == 3) {
        Try {
          val path = file(arr(0))
          val span = Span(arr(1).toLong, arr(2).toLong)
          ExtAudioRegionDrag(document, path, span)
        } .toOption
      } else None
    }

    private def acceptAndUpdate(e: DropTargetDragEvent, drag: Drag[S]): Unit = {
      val loc   = e.getLocation
      val drop  = mkDrop(drag, loc)
      updateDnD(Some(drop))
      e.acceptDrag(e.getDropAction) // COPY
    }

    private def isSupported(t: Transferable): Boolean =
      t.isDataFlavorSupported(DnD.flavor) ||
      t.isDataFlavorSupported(DataFlavor.stringFlavor)

    private def mkDrag(t: Transferable, isDragging: Boolean): Option[Drag[S]] =
      if (t.isDataFlavorSupported(DnD.flavor)) {
        t.getTransferData(DnD.flavor) match {
          case d: DnD.Drag[_] if d.document == document => Some(d.asInstanceOf[DnD.Drag[S]])
          case _ => None
        }

      } else if (t.isDataFlavorSupported(DataFlavor.stringFlavor)) {
        mkExtStringDrag(t, isDragging = isDragging)

      } else {
        None
      }

    private def process(e: DropTargetDragEvent): Unit = {
      val d = mkDrag(e.getTransferable, isDragging = true)
      // println(s"mkDrag = $d")
      d match {
        case Some(drag) => acceptAndUpdate(e, drag)
        case _          => abortDrag(e)
      }
    }

    def drop(e: DropTargetDropEvent): Unit = {
      updateDnD(None)

      val t = e.getTransferable
      if (!isSupported(t)) {
        e.rejectDrop()
        return
      }
      e.acceptDrop(e.getDropAction)
      mkDrag(t, isDragging = false) match {
        case Some(drag) =>
          val loc     = e.getLocation
          val drop    = mkDrop(drag, loc)
          val success = acceptDnD(drop)
          e.dropComplete(success)

        case _ => e.rejectDrop()
      }
    }
  }

  new DropTarget(peer, COPY | LINK, Adaptor)
}
