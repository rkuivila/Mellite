/*
 *  ElementsFrameImpl.scala
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
package document

import scala.swing.Action
import de.sciss.lucre.stm
import de.sciss.synth.proc.Folder
import de.sciss.desktop.{UndoManager, Menu}
import de.sciss.file._
import de.sciss.swingplus.PopupMenu
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.swing._
import de.sciss.synth.proc
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.mellite.gui.edit.EditRemoveObj
import javax.swing.undo.{CompoundEdit, UndoableEdit}
import proc.FolderElem
import de.sciss.mellite.gui.impl.component.{CollectionViewImpl, CollectionFrameImpl}

object ElementsFrameImpl {
  def apply[S <: Sys[S], S1 <: Sys[S1]](doc: Workspace[S], nameOpt: Option[Expr[S1, String]])(implicit tx: S#Tx,
                                        cursor: stm.Cursor[S], bridge: S#Tx => S1#Tx): DocumentElementsFrame[S] = {
    implicit val undoMgr  = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val folderView      = FolderView(doc.folder, doc.root())
    val name0           = nameOpt.map(_.value(bridge(tx)))
    val view            = new ViewImpl[S, S1](doc, folderView) {
      protected val nameObserver = nameOpt.map { name =>
        name.changed.react { implicit tx => upd =>
          deferTx(nameUpdate(Some(upd.now)))
        } (bridge(tx))
      }

      //      deferTx {
      //        guiInit()
      //        nameUpdate(name0)
      //      }
    }
    view.init()

    val res = new FrameImpl[S](view, name0 = name0)
    res.init()
    res
  }

  private final class FrameImpl[S <: Sys[S]](view: ViewImpl[S, _], name0: Option[String])
    extends CollectionFrameImpl[S](view) with DocumentElementsFrame[S] {

    override protected def initGUI(): Unit = {
      view.addListener {
        case CollectionViewImpl.NamedChanged(n) => title = n
      }
      view.nameUpdate(name0)
    }
  }

  private abstract class ViewImpl[S <: Sys[S], S1 <: Sys[S1]](val workspace: Workspace[S],
                                                              val peer: FolderView[S])
                                       (implicit val cursor: stm.Cursor[S], undoManager: UndoManager,
                                        protected val bridge: S#Tx => S1#Tx)
    extends CollectionViewImpl[S, S1] // (document, file = Some(document.folder), frameY = 0f)
    {

    impl =>

    protected def nameObserver: Option[stm.Disposable[S1#Tx]]

    protected def mkTitle(sOpt: Option[String]): String =
      s"${workspace.folder.base}${sOpt.fold("")(s => s"/$s")} : Elements"

    private final class AddAction(f: ObjView.Factory) extends Action(f.prefix) {
      icon = f.icon

      def apply(): Unit = {
        implicit val folderSer = Folder.serializer[S]
        val parentH = cursor.step { implicit tx => tx.newHandle(impl.peer.insertionPoint._1) }
        f.initDialog[S](parentH, None /* XXX TODO: Some(window) */).foreach(undoManager.add)
      }
    }

    private lazy val addPopup: PopupMenu = {
      import Menu._
      val pop = Popup()
      ObjView.factories.toList.sortBy(_.prefix).foreach { f =>
        pop.add(Item(f.prefix, new AddAction(f)))
      }

      val window = GUI.findWindow(component).getOrElse(sys.error(s"No window for $impl"))
      val res = pop.create(window)
      res.peer.pack() // so we can read `size` correctly
      res
    }

    final protected lazy val actionAdd: Action = Action(null) {
      val bp = ggAdd
      addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
    }

    final protected lazy val actionDelete: Action = Action(null) {
      val sel = peer.selection
      val edits: List[UndoableEdit] = cursor.step { implicit tx =>
        sel.map { nodeView =>
          val parent = nodeView.parentOption.flatMap { pView =>
            pView.modelData() match {
              case FolderElem.Obj(objT) => Some(objT.elem.peer)
              case _ => None
            }
          }.getOrElse(peer.root())
          val childH  = nodeView.modelData
          val child   = childH()
          val idx     = parent.indexOf(child)
          implicit val folderSer = Folder.serializer[S]
          val parentH = tx.newHandle(parent)
          EditRemoveObj[S](nodeView.renderData.prefix, parentH, idx, childH)
        }
      }
      edits match {
        case single :: Nil => undoManager.add(single)
        case Nil =>
        case several =>
          val ce = new CompoundEdit
          several.foreach(ce.addEdit)
          ce.end()
          undoManager.add(ce)
      }
    }

    final protected def initGUI2(): Unit = {
      peer.addListener {
        case FolderView.SelectionChanged(_, sel) =>
          selectionChanged(sel.map(_.renderData))
      }
    }

    protected def selectedObjects: List[ObjView[S]] = peer.selection.map(_.renderData)
  }
}