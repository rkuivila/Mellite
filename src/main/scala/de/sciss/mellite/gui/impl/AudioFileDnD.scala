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

  final case class Drag[S <: Sys[S]](document: Document[S], source: stm.Source[S#Tx, AudioGrapheme[S]],
                                     grapheme: Grapheme.Value.Audio, selection: Span,
                                     bus: Option[stm.Source[S#Tx, Element.Int[S]]])
  final case class Drop[S <: Sys[S]](frame: Long, y: Int, drag: Drag[S])

  //  private[AudioFileDnD] final class Transferable[S <: Sys[S]](data: Drag[S])
  //  extends datatransfer.Transferable {
  //    // println(s"New Transferable. Data = $data")
  //
  //    def getTransferDataFlavors: Array[DataFlavor] = Array(AudioFileDnD.flavor)
  //    def isDataFlavorSupported(flavor: DataFlavor): Boolean = {
  //      val res = flavor == AudioFileDnD.flavor
  //      // println(s"isDataFlavorSupported($flavor) == $res")
  //      res
  //    }
  //    def getTransferData(flavor: DataFlavor): AnyRef = data
  //  }

  final val flavor = DragAndDrop.internalFlavor[Drag[_]]

  final class Button[S <: Sys[S]](document: Document[S], val source: stm.Source[S#Tx, AudioGrapheme[S]],
                                  snapshot0: Grapheme.Value.Audio,
                                  timelineModel: TimelineModel)
    extends swing.Button("Region") {

    var snapshot  = snapshot0
    var bus       = Option.empty[stm.Source[S#Tx, Element.Int[S]]]

    private object Transfer extends TransferHandler {
      override def getSourceActions(c: JComponent): Int =
        if (timelineModel.selection.isEmpty) TransferHandler.NONE else COPY

      override def createTransferable(c: JComponent): datatransfer.Transferable =
        DragAndDrop.Transferable(flavor) {
          timelineModel.selection match {
            case sp @ Span(_, _) if sp.nonEmpty =>
              Drag(document, source, grapheme = snapshot, selection = sp, bus = bus)
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

  protected def document: Document[S]
  protected def timelineModel: TimelineModel
  
  protected def updateDnD(drop: Option[Drop[S]]): Unit
  protected def acceptDnD(drop:        Drop[S] ): Boolean

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

    private def mkDrop(d: AudioFileDnD.Drag[S], loc: Point): Drop[S] = {
      val visi  = timelineModel.visible
      val w     = peer.getWidth
      val frame = (loc.x.toDouble / w * visi.length + visi.start).toLong
      val y     = loc.y
      Drop(frame = frame, y = y, drag = d)
    }
  
    private def process(e: DropTargetDragEvent) {
      val t = e.getTransferable
      if (!t.isDataFlavorSupported(AudioFileDnD.flavor)) {
        println("NO WAY JOSE")
        abortDrag(e)
  
      } else t.getTransferData(AudioFileDnD.flavor) match {
        case d: AudioFileDnD.Drag[_] if (d.document == document) =>
          val loc     = e.getLocation
          val drag    = d.asInstanceOf[AudioFileDnD.Drag[S]]
          val drop    = mkDrop(drag, loc)
          updateDnD(Some(drop))
          e.acceptDrag(COPY)

        case _ =>
          println("HUH?")
          abortDrag(e)
      }
    }

    def drop(e: DropTargetDropEvent) {
      updateDnD(None)

      val t = e.getTransferable
      if (!t.isDataFlavorSupported(AudioFileDnD.flavor)) {
        e.rejectDrop()
  
      } else t.getTransferData(AudioFileDnD.flavor) match {
        case d: AudioFileDnD.Drag[_] =>
          val drag    = d.asInstanceOf[AudioFileDnD.Drag[S]]
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
