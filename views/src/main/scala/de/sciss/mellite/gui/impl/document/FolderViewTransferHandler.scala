package de.sciss.mellite
package gui
package impl.document

import java.awt.datatransfer.{DataFlavor, Transferable}
import java.io.File
import javax.swing.TransferHandler.TransferSupport
import javax.swing.undo.UndoableEdit
import javax.swing.{JComponent, TransferHandler}

import de.sciss.desktop.UndoManager
import de.sciss.desktop.edit.CompoundEdit
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Txn, Obj, Sys}
import de.sciss.lucre.swing.TreeTableView
import de.sciss.mellite.gui.edit.{EditFolderInsertObj, EditFolderRemoveObj}
import de.sciss.synth.io.{AudioFile, AudioFileSpec}
import de.sciss.synth.proc._
import org.scalautils.TypeCheckedTripleEquals

import scala.language.existentials
import scala.util.Try

/** Mixin that provides a transfer handler for the folder view. */
trait FolderViewTransferHandler[S <: Sys[S]] { fv =>
  protected def workspace       : Workspace[S]
  protected def undoManager     : UndoManager
  protected implicit def cursor : stm.Cursor[S]

  protected def treeView: TreeTableView[S, Obj[S], Folder[S], ListObjView[S]]
  protected def selection: FolderView.Selection[S]

  protected def findLocation(f: File): Option[ActionArtifactLocation.QueryResult[S]]

  protected object FolderTransferHandler extends TransferHandler {
    // ---- export ----

    override def getSourceActions(c: JComponent): Int =
      TransferHandler.COPY | TransferHandler.MOVE | TransferHandler.LINK // dragging only works when MOVE is included. Why?

    override def createTransferable(c: JComponent): Transferable = {
      val sel     = selection
      val trans0  = DragAndDrop.Transferable(FolderView.SelectionFlavor) {
        new FolderView.SelectionDnDData[S](fv.workspace, fv.cursor, sel)
      }
      val trans1 = if (sel.size == 1) {
        val _res = DragAndDrop.Transferable(ListObjView.Flavor) {
          new ListObjView.Drag[S](fv.workspace, sel.head.renderData)
        }
        DragAndDrop.Transferable.seq(trans0, _res)
      } else trans0

      trans1
    }

    // ---- import ----
    override def canImport(support: TransferSupport): Boolean =
      treeView.dropLocation match {
        case Some(tdl) =>
          val locOk = tdl.index >= 0 || (tdl.column == 0 && !tdl.path.lastOption.exists(_.isLeaf))
          if (locOk) {
            val isSelection = support.isDataFlavorSupported(FolderView.SelectionFlavor)
            if (isSelection) {
              val data = support.getTransferable.getTransferData(FolderView.SelectionFlavor)
                .asInstanceOf[FolderView.SelectionDnDData[_]]
              if (data.workspace != workspace) {
                // no linking between sessions
                support.setDropAction(TransferHandler.COPY)
              }
              true
            } else {
              support.isDataFlavorSupported(DataFlavor.javaFileListFlavor)
            }

          } else {
            false
          }

        case _ => false
      }

    // XXX TODO: not sure whether removal should be in exportDone or something
    private def insertData(sel: FolderView.Selection[S], newParent: Folder[S], idx: Int, dropAction: Int)
                          (implicit tx: S#Tx): Option[UndoableEdit] = {
      // println(s"insert into $parent at index $idx")

      def isNested(c: Obj[S]): Boolean = c match {
        case objT: Folder[S] =>
          import TypeCheckedTripleEquals._
          objT === newParent || objT.iterator.toList.exists(isNested)
        case _ => false
      }

      import TypeCheckedTripleEquals._
      val isMove  = dropAction === TransferHandler.MOVE
      val isCopy  = dropAction === TransferHandler.COPY

      // make sure we are not moving a folder within itself (it will magically disappear :)
      val sel1 = if (!isMove) sel else sel.filterNot(nv => isNested(nv.modelData()))

      // if we move children within the same folder, adjust the insertion index by
      // decrementing it for any child which is above the insertion index, because
      // we will first remove all children, then re-insert them.
      val idx0 = if (idx >= 0) idx else newParent /* .children */.size
      val idx1 = if (!isMove) idx0 else idx0 - sel1.count { nv =>
        val isInNewParent = nv.parent === newParent
        val child = nv.modelData()
        isInNewParent && newParent.indexOf(child) <= idx0
      }
      // println(s"idx0 $idx0 idx1 $idx1")

      implicit val folderSer = Folder.serializer[S]

      val editRemove: List[UndoableEdit] = if (!isMove) Nil else sel1.flatMap { nv =>
        val parent: Folder[S] = nv.parent
        val childH  = nv.modelData
        val idx     = parent.indexOf(childH())
        if (idx < 0) {
          println("WARNING: Parent of drag object not found")
          None
        } else {
          val edit = EditFolderRemoveObj[S](nv.renderData.humanName, parent, idx, childH())
          Some(edit)
        }
      }

      val editInsert = sel1.zipWithIndex.map { case (nv, off) =>
        val childH  = nv.modelData
        val child0  = childH()
        val child   = if (!isCopy) child0 else Obj.copy(child0)
        EditFolderInsertObj[S](nv.renderData.humanName, newParent, idx1 + off, child)
      }
      val edits: List[UndoableEdit] = editRemove ++ editInsert
      val name = sel1 match {
        case single :: Nil  => single.renderData.humanName
        case _              => "Elements"
      }

      val prefix = if (isMove) "Move" else if (isCopy) "Copy" else "Link"
      CompoundEdit(edits, s"$prefix $name")
    }

    private def importSelection(support: TransferSupport, parent: Folder[S], index: Int)
                               (implicit tx: S#Tx): Option[UndoableEdit] = {
      val data = support.getTransferable.getTransferData(FolderView.SelectionFlavor)
        .asInstanceOf[FolderView.SelectionDnDData[S]]

      // we have performed this check already before:
      // if (data.workspace === workspace) {
        insertData(data.selection, parent, idx = index, dropAction = support.getDropAction)
      // } else {
      //  None
      // }
    }

    private def copyData(support: TransferSupport): Option[UndoableEdit] = {
      // cf. https://stackoverflow.com/questions/20982681
      val data  = support.getTransferable.getTransferData(FolderView.SelectionFlavor)
        .asInstanceOf[FolderView.SelectionDnDData[In] forSome { type In <: Sys[In] }]
      copyData1(data)
    }

    private def copyData1[In <: Sys[In]](data: FolderView.SelectionDnDData[In]): Option[UndoableEdit] = {
      Txn.copy[In, S, Option[UndoableEdit]] { (txIn: In#Tx, tx: S#Tx) => {
        parentOption(tx).flatMap { case (parent, idx) =>
          copyData2(data.selection, parent, idx)(txIn, tx)
        }
      }} (data.cursor, fv.cursor)
    }

    private def copyData2[In <: Sys[In]](sel: FolderView.Selection[In], newParent: Folder[S], idx: Int)
                                       (implicit txIn: In#Tx, tx: S#Tx): Option[UndoableEdit] = {
      val idx1 = if (idx >= 0) idx else newParent.size

      implicit val folderSer = Folder.serializer[S]

//      sel.map { nv =>
//        val childH = nv.modelData
//      }

      val editInsert = sel.zipWithIndex.map { case (nv, off) =>
        val childH  = nv.modelData
        val child0  = childH()
        val child   = Obj.copy[In, S, Obj](child0)
        EditFolderInsertObj[S](nv.renderData.humanName, newParent, idx1 + off, child)
      }
      val edits: List[UndoableEdit] = editInsert
      val name = sel match {
        case single :: Nil  => single.renderData.humanName
        case _              => "Elements"
      }

      val prefix = "Copy"
      CompoundEdit(edits, s"$prefix $name")
    }

    private def importFiles(support: TransferSupport, parent: Folder[S], index: Int)
                           (implicit tx: S#Tx): Option[UndoableEdit] = {
      import scala.collection.JavaConversions._
      val data: List[File] = support.getTransferable.getTransferData(DataFlavor.javaFileListFlavor)
        .asInstanceOf[java.util.List[File]].toList
      val tup: List[(File, AudioFileSpec)] = data.flatMap { f =>
        Try(AudioFile.readSpec(f)).toOption.map(f -> _)
      }
      val trip: List[(File, AudioFileSpec, ActionArtifactLocation.QueryResult[S])] =
        tup.flatMap { case (f, spec) =>
          findLocation(f).map { loc => (f, spec, loc) }
        }

      implicit val folderSer = Folder.serializer[S]
      // damn, this is annoying threading of state
      val (_, edits: List[UndoableEdit]) = ((index, List.empty[UndoableEdit]) /: trip) {
        case ((idx0, list0), (f, spec, either)) =>
          ActionArtifactLocation.merge(either).fold((idx0, list0)) { case (xs, locM) =>
            val (idx2, list2) = ((idx0, list0) /: xs) { case ((idx1, list1), x) =>
              val edit1 = EditFolderInsertObj[S]("Location", parent, idx1, x)
              (idx1 + 1, list0 :+ edit1)
            }
            val obj   = ObjectActions.mkAudioFile(locM, f, spec)
            val edit2 = EditFolderInsertObj[S]("Audio File", parent, idx2, obj)
            (idx2 + 1, list2 :+ edit2)
          }
      }
      CompoundEdit(edits, "Insert Audio Files")
    }

    private def parentOption(implicit tx: S#Tx): Option[(Folder[S], Int)] =
      treeView.dropLocation.flatMap { tdl =>
        val parentOpt = tdl.path.lastOption.fold(Option(treeView.root())) { nodeView =>
          nodeView.modelData() match {
            case objT: Folder[S]  => Some(objT)
            case _                => None
          }
        }
        parentOpt.map(_ -> tdl.index)
      }

    override def importData(support: TransferSupport): Boolean =
      treeView.dropLocation.exists { tdl =>
        val editOpt = {
          val crossSession = support.isDataFlavorSupported(FolderView.SelectionFlavor) &&
            (support.getTransferable.getTransferData(FolderView.SelectionFlavor)
              .asInstanceOf[FolderView.SelectionDnDData[_]].workspace != workspace)

          if (crossSession)
            copyData(support)
          else cursor.step { implicit tx =>
            parentOption.flatMap { case (parent,idx) =>
              if (support.isDataFlavorSupported(FolderView.SelectionFlavor))
                importSelection(support, parent, idx)
              else if (support.isDataFlavorSupported(DataFlavor.javaFileListFlavor))
                importFiles(support, parent, idx)
              else None
            }
          }
        }
        editOpt.foreach(undoManager.add)
        editOpt.isDefined
      }
  }
}
