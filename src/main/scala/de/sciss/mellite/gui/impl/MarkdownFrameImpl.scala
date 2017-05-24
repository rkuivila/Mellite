/*
 *  MarkdownFrameImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{OptionPane, UndoManager}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.{CellView, View}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{Markdown, Workspace}

import scala.collection.immutable.{Seq => ISeq}

object MarkdownFrameImpl {
  def apply[S <: Sys[S]](obj: Markdown[S], bottom: ISeq[View[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): MarkdownFrame[S] = {
    implicit val undo = new UndoManagerImpl
    make[S](obj, obj, bottom = bottom)
  }

  private def make[S <: Sys[S]](pObj: Obj[S], obj: Markdown[S], bottom: ISeq[View[S]])
                               (implicit tx: S#Tx, ws: Workspace[S], csr: stm.Cursor[S],
                                undoMgr: UndoManager): MarkdownFrame[S] = {
    // val _name   = /* title getOrElse */ obj.attr.name
    val codeView  = MarkdownView(obj, bottom = bottom)
    val view = codeView
//    val view      = rightViewOpt.fold[View[S]](codeView) { bottomView =>
//      new View.Editable[S] with ViewHasWorkspace[S] {
//        val undoManager: UndoManager  = undoMgr
//        val cursor: stm.Cursor[S]     = csr
//        val workspace: Workspace[S]   = ws
//
//        lazy val component: Component = {
//          val res = new SplitPane(Orientation.Vertical, codeView.component, bottomView.component)
//          res.oneTouchExpandable  = true
//          res.resizeWeight        = 1.0
//          // cf. https://stackoverflow.com/questions/4934499
//          res.peer.addAncestorListener(new AncestorListener {
//            def ancestorAdded  (e: AncestorEvent): Unit = res.dividerLocation = 1.0
//            def ancestorRemoved(e: AncestorEvent): Unit = ()
//            def ancestorMoved  (e: AncestorEvent): Unit = ()
//          })
//          res
//        }
//
//        def dispose()(implicit tx: S#Tx): Unit = {
//          codeView  .dispose()
//          bottomView.dispose()
//        }
//      }
//    }
    val _name = AttrCellView.name(pObj)
    val res = new FrameImpl(codeView = codeView, view = view, name = _name)
    res.init()
  }

  // ---- frame impl ----

  private final class FrameImpl[S <: Sys[S]](val codeView: MarkdownView[S], val view: View[S],
                                             name: CellView[S#Tx, String])
    extends WindowImpl[S](name) with MarkdownFrame[S] {

    override protected def checkClose(): Boolean =
      !codeView.dirty || {
        val message = "The text has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close - $title"
        opt.show(Some(window)) match {
          case OptionPane.Result.No => true
          case OptionPane.Result.Yes =>
            /* val fut = */ codeView.save()
            true

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            false
        }
      }
  }
}