/*
 *  FolderFrameImpl.scala
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

import javax.swing.undo.UndoableEdit

import de.sciss.desktop.edit.CompoundEdit
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.file._
import de.sciss.lucre.stm
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.{EditFolderInsertObj, EditFolderRemoveObj}
import de.sciss.mellite.gui.impl.component.{CollectionFrameImpl, CollectionViewImpl}
import de.sciss.swingplus.PopupMenu
import de.sciss.synth.proc.{Obj, Folder, FolderElem}

import scala.swing.Action
import scala.collection.breakOut

object FolderFrameImpl {
  def apply[S <: Sys[S], S1 <: Sys[S1]](nameObs: ExprView[S1#Tx, Option[String]],
                                        folder: Folder[S],
                                        isWorkspaceRoot: Boolean)(implicit tx: S#Tx,
                                        workspace: Workspace[S], cursor: stm.Cursor[S],
                                        bridge: S#Tx => S1#Tx): FolderFrame[S] = {
    implicit val undoMgr  = new UndoManagerImpl
    val folderView      = FolderView(folder)
    val name0           = nameObs()(bridge(tx))
    val view            = new ViewImpl[S, S1](folderView) {
      protected val nameObserver = nameObs.react { implicit tx => now =>
        deferTx(nameUpdate(now))
      } (bridge(tx))
    }
    view.init()

    val res = new FrameImpl[S](view, name0 = name0, isWorkspaceRoot = isWorkspaceRoot)
    res.init()
    res
  }

  private final class FrameImpl[S <: Sys[S]](_view: ViewImpl[S, _], name0: Option[String], isWorkspaceRoot: Boolean)
    extends CollectionFrameImpl[S](_view) with FolderFrame[S] {

    def workspace = _view.workspace

    override protected def initGUI(): Unit = {
      _view.addListener {
        case CollectionViewImpl.NamedChanged(n) => title = n
      }
      _view.nameUpdate(name0)
    }

    override protected def placement: (Float, Float, Int) = (0.5f, 0.0f, 20)

    override protected def performClose(): Unit = if (isWorkspaceRoot) {
      log(s"Closing workspace ${workspace.folder}")
      Application.documentHandler.removeDocument(workspace)
      workspace.close()
    } else {
      super.performClose()
    }
  }

  abstract class ViewImpl[S <: Sys[S], S1 <: Sys[S1]](val peer: FolderView[S])
                                       (implicit val workspace: Workspace[S],
                                        val cursor: stm.Cursor[S], val undoManager: UndoManager,
                                        protected val bridge: S#Tx => S1#Tx)
    extends CollectionViewImpl[S, S1]
    {

    impl =>

    protected def nameObserver: stm.Disposable[S1#Tx]

    protected def mkTitle(sOpt: Option[String]): String =
      s"${workspace.folder.base}${sOpt.fold("")(s => s"/$s")} : Elements"

    protected type InsertConfig = Unit

    protected def prepareInsert(f: ObjView.Factory): Option[InsertConfig] = Some(())

    protected def editInsert(f: ObjView.Factory, xs: List[Obj[S]], config: Unit)(implicit tx: S#Tx): Option[UndoableEdit] = {
      val (parent, idx) = impl.peer.insertionPoint
      val edits: List[UndoableEdit] = xs.zipWithIndex.map { case (x, j) =>
        EditFolderInsertObj(f.prefix, parent, idx + j, x)
      } (breakOut)
      CompoundEdit(edits, "Create Objects")
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
          EditFolderRemoveObj[S](nodeView.renderData.prefix, parent, idx, child)
        }
      }
      val ceOpt = CompoundEdit(edits, "Delete Elements")
      ceOpt.foreach(undoManager.add)
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