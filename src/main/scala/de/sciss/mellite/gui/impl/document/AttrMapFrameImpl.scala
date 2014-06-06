/*
 *  AttrMapFrameImpl.scala
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

package de.sciss.mellite.gui
package impl
package document

import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import scala.swing.Component
import de.sciss.desktop.{DialogSource, Window}

object AttrMapFrameImpl {
  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): AttrMapFrame[S] = {
    ???
  }
//
//  private final class Impl[S <: Sys[S]] extends AttrMapFrame[S] with WindowHolder[Frame[S]] {
//
//
//    def frameClosing(): Unit = {
//      ???
//    }
//  }
//
//  private final class Frame[S <: Sys[S]](view: Impl[S], _contents: Component) extends WindowImpl {
//    // file            = Some(view.document.folder)
//    closeOperation  = Window.CloseDispose
//    reactions += {
//      case Window.Closing(_) => view.frameClosing()
//    }
//
//    bindMenus(
//      "edit.undo" -> view.contents.undoManager.undoAction,
//      "edit.redo" -> view.contents.undoManager.redoAction
//    )
//
//    contents = _contents
//
//    pack()
//    // centerOnScreen()
//    GUI.placeWindow(this, 0.5f, 0.25f, 24)
//
//    def show[A](source: DialogSource[A]): A = showDialog(source)
//  }
}