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
package audiofile

import de.sciss.span.Span
import java.awt.datatransfer
import de.sciss.synth.proc.{Grapheme}
import de.sciss.lucre.stm
import javax.swing.{JComponent, ImageIcon, TransferHandler}
import java.awt.event.{MouseEvent, MouseAdapter}
import TransferHandler.COPY
import de.sciss.audiowidgets.TimelineModel
import de.sciss.mellite.gui.impl.timeline
import de.sciss.mellite.Element.AudioGrapheme
import de.sciss.lucre.synth.Sys

object DnD {
  // XXX TODO: should carry document to avoid cross-document DnD without deep copy


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

  import timeline.DnD.{AudioDrag => Drag, flavor}

  // final val flavor = DragAndDrop.internalFlavor[Drag[_]]

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
              Drag(document, source, /* grapheme = snapshot, */ selection = sp, bus = bus)
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
      override def mousePressed(e: MouseEvent): Unit = {
        dndInitX	  = e.getX
        dndInitY    = e.getY
        dndPressed  = true
        dndStarted	= false
      }

      override def mouseReleased(e: MouseEvent): Unit = {
        dndPressed  = false
        dndStarted	= false
      }

      override def mouseDragged(e: MouseEvent): Unit =
        if (dndPressed && !dndStarted && ((math.abs(e.getX - dndInitX) > 5) || (math.abs(e.getY - dndInitY) > 5))) {
          Transfer.exportAsDrag(peer, e, COPY)
          dndStarted = true
        }
    }

    peer.addMouseListener      (Mouse)
    peer.addMouseMotionListener(Mouse)
  }
}