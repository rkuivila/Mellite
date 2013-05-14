package de.sciss.mellite
package gui
package impl

import java.awt.dnd.{DropTarget, DropTargetEvent, DropTargetDropEvent, DropTargetDragEvent, DropTargetAdapter}
import de.sciss.span.Span
import java.awt.{Point, datatransfer}
import datatransfer.DataFlavor
import de.sciss.synth.proc.{Grapheme, Sys}
import de.sciss.lucre.stm
import Element.AudioGrapheme
import javax.swing.{JComponent, ImageIcon, TransferHandler}
import java.awt.event.{MouseEvent, MouseAdapter}
import scala.swing.Component
import TransferHandler.COPY

object AudioFileDnD {
  // XXX TODO: should carry document to avoid cross-document DnD without deep copy

  final case class Drag(grapheme: Grapheme.Value.Audio, selection: Span)
  final case class Data[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, AudioGrapheme[S]], drag: Drag)
  final case class Drop(frame: Long, y: Int, drag: Drag)

  private[AudioFileDnD] final class Transferable[S <: Sys[S]](data: Data[S])
  extends datatransfer.Transferable {
    // println(s"New Transferable. Data = $data")

    def getTransferDataFlavors: Array[DataFlavor] = Array(AudioFileDnD.flavor)
    def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
      val res = flavor == AudioFileDnD.flavor
      // println(s"isDataFlavorSupported($flavor) == $res")
      res
    }
    def getTransferData(flavor: DataFlavor): AnyRef = data
  }

  // XXX TODO: hmmm. this is all a bit odd. Should be like Bus DnD in audio file view.
  final val flavor = new DataFlavor(classOf[Transferable[_]], "AudioRegion")

  final class Button[S <: Sys[S]](document: Document[S], val source: stm.Source[S#Tx, AudioGrapheme[S]],
                                  snapshot0: Grapheme.Value.Audio,
                                  timelineModel: TimelineModel)
    extends swing.Button("Region") {

    var snapshot = snapshot0

    private object Transfer extends TransferHandler {
      override def getSourceActions(c: JComponent): Int =
        if (timelineModel.selection.isEmpty) TransferHandler.NONE else COPY

      override def createTransferable(c: JComponent): datatransfer.Transferable = {
        timelineModel.selection match {
          case sp @ Span(_, _) if sp.nonEmpty =>
            new Transferable(Data(document, source, Drag(grapheme = snapshot, selection = sp)))
          case _ => null
        }
      }
    }

    focusable = false
    icon      = new ImageIcon(Mellite.getClass.getResource("dragicon.png"))
    peer.setTransferHandler(Transfer)

    private var dndInitX    = 0
    private var dndInitY    = 0
    private var dndPressed  = false
    private var dndStarted  = false

    private object Mouse extends MouseAdapter {
      override def mousePressed(e: MouseEvent) {
        dndInitX	  = e.getX
        dndInitY    = e.getY
        dndPressed  = true
        dndStarted	= false
      }

      override def mouseReleased(e: MouseEvent) {
        dndPressed  = false
        dndStarted	= false
      }

      override def mouseDragged(e: MouseEvent) {
        if (dndPressed && !dndStarted && ((math.abs(e.getX - dndInitX) > 5) || (math.abs(e.getY - dndInitY) > 5))) {
          Transfer.exportAsDrag(peer, e, COPY)
          dndStarted = true
        }
      }
    }

    peer.addMouseListener(Mouse)
    peer.addMouseMotionListener(Mouse)
  }
}
trait AudioFileDnD[S <: Sys[S]] {
  _: Component =>
  
  import AudioFileDnD._

  protected def timelineModel: TimelineModel
  
  protected def updateDnD(drop: Option[Drop]): Unit
  protected def acceptDnD(drop: Drop, data: Data[S]): Boolean

  private object Adaptor extends DropTargetAdapter {
    override def dragEnter(e: DropTargetDragEvent) {
      process(e)
    }

    override def dragOver(e: DropTargetDragEvent) {
      process(e)
    }

    override def dragExit(e: DropTargetEvent) {
      updateDnD(None)
    }

    private def abortDrag(e: DropTargetDragEvent) {
      updateDnD(None)
      e.rejectDrag()
    }

    private def mkDrop(d: AudioFileDnD.Data[_], loc: Point): Drop = {
      val visi  = timelineModel.visible
      val w     = peer.getWidth
      val frame = (loc.x.toDouble / w * visi.length + visi.start).toLong
      val y     = loc.y
      Drop(frame = frame, y = y, drag = d.drag)
    }
  
    private def process(e: DropTargetDragEvent) {
      val t = e.getTransferable
      if (!t.isDataFlavorSupported(AudioFileDnD.flavor)) {
        abortDrag(e)
  
      } else t.getTransferData(AudioFileDnD.flavor) match {
        case d: AudioFileDnD.Data[_] =>
          val loc   = e.getLocation
          val drop  = mkDrop(d, loc)
          updateDnD(Some(drop))
          e.acceptDrag(COPY)

        case _ =>
          abortDrag(e)
      }
    }

    def drop(e: DropTargetDropEvent) {
      updateDnD(None)

      val t = e.getTransferable
      if (!t.isDataFlavorSupported(AudioFileDnD.flavor)) {
        e.rejectDrop()
  
      } else t.getTransferData(AudioFileDnD.flavor) match {
        case d: AudioFileDnD.Data[_] =>
          val data    = d.asInstanceOf[AudioFileDnD.Data[S]]
          e.acceptDrop(COPY)
          val loc     = e.getLocation
          val drop    = mkDrop(d, loc)
          val success = acceptDnD(drop, data)
          e.dropComplete(success)

        case _ =>
          e.rejectDrop()
      }
    }
  }

  new DropTarget(peer, COPY, Adaptor)
}
