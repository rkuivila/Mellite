package de.sciss
package mellite
package gui
package impl

import javax.swing.TransferHandler._
import java.awt.dnd.{DropTarget, DropTargetDropEvent, DropTargetEvent, DropTargetDragEvent, DropTargetAdapter}
import de.sciss.synth.proc.Sys
import scala.swing.Component
import java.awt.Point
import de.sciss.lucre.stm
import de.sciss.mellite.Element.AudioGrapheme
import de.sciss.span.Span
import de.sciss.audiowidgets.TimelineModel

object TimelineDnD {
  sealed trait Drag[S <: Sys[S]] {
    def document: Document[S]
    def source: stm.Source[S#Tx, Element[S]]
  }
  final case class AudioDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, AudioGrapheme[S]],
                                          /* grapheme: Grapheme.Value.Audio, */ selection: Span,
                                          bus: Option[stm.Source[S#Tx, Element.Int[S]]])
    extends Drag[S]

  final case class IntDrag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, Element.Int[S]]) extends Drag[S]

  final case class Drop[S <: Sys[S]](frame: Long, y: Int, drag: Drag[S])

  final val flavor = DragAndDrop.internalFlavor[Drag[_]]
}
trait TimelineDnD[S <: Sys[S]] {
  _: Component =>

  import TimelineDnD._

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

    private def mkDrop(d: TimelineDnD.Drag[S], loc: Point): Drop[S] = {
      val visi  = timelineModel.visible
      val w     = peer.getWidth
      val frame = (loc.x.toDouble / w * visi.length + visi.start).toLong
      val y     = loc.y
      Drop(frame = frame, y = y, drag = d)
    }

    private def process(e: DropTargetDragEvent): Unit = {
      val t = e.getTransferable
      if (!t.isDataFlavorSupported(TimelineDnD.flavor)) {
        abortDrag(e)

      } else t.getTransferData(TimelineDnD.flavor) match {
        case d: TimelineDnD.Drag[_] if d.document == document =>
          val loc     = e.getLocation
          val drag    = d.asInstanceOf[TimelineDnD.Drag[S]]
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
      if (!t.isDataFlavorSupported(TimelineDnD.flavor)) {
        e.rejectDrop()

      } else t.getTransferData(TimelineDnD.flavor) match {
        case d: TimelineDnD.Drag[_] =>
          val drag    = d.asInstanceOf[TimelineDnD.Drag[S]]
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
