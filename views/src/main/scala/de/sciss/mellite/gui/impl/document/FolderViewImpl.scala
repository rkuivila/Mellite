/*
 *  FolderViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.desktop.edit.CompoundEdit
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.{Expr, String => StringEx}
import de.sciss.synth.proc.{ObjKeys, FolderElem, Folder, Obj, StringElem}
import swing.Component
import scala.collection.{JavaConversions, breakOut}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.model.impl.ModelImpl
import scala.util.control.NonFatal
import javax.swing.{CellEditor, DropMode, JComponent, TransferHandler}
import java.awt.datatransfer.{Transferable, DataFlavor}
import javax.swing.TransferHandler.TransferSupport
import de.sciss.treetable.TreeTableSelectionChanged
import de.sciss.treetable.TreeTableCellRenderer
import java.io.File
import de.sciss.lucre.stm
import de.sciss.model.Change
import de.sciss.lucre.swing.{TreeTableView, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Folder.Update
import de.sciss.lucre.swing.TreeTableView.ModelUpdate
import de.sciss.treetable.j.{TreeTableCellEditor, DefaultTreeTableCellEditor}
import javax.swing.event.{ChangeEvent, CellEditorListener}
import de.sciss.desktop.UndoManager
import de.sciss.mellite.gui.edit.{EditFolderInsertObj, EditFolderRemoveObj, EditAttrMap}
import de.sciss.lucre
import de.sciss.synth.io.{AudioFileSpec, AudioFile}
import scala.util.Try
import javax.swing.undo.UndoableEdit

object FolderViewImpl {
  // private final val DEBUG = false

  // TreeTableViewImpl.DEBUG = true

  def apply[S <: Sys[S]](root0: Folder[S])
                        (implicit tx: S#Tx, workspace: Workspace[S],
                         cursor: stm.Cursor[S], undoManager: UndoManager): FolderView[S] = {
    implicit val folderSer = Folder.serializer[S]

    new Impl[S] {
      val mapViews  = tx.newInMemoryIDMap[ObjView[S]]  // folder IDs to renderers
      val treeView  = TreeTableView[S, Obj[S], Folder[S], Folder.Update[S], ObjView[S]](root0, TTHandler)

      deferTx {
        guiInit()
      }
    }
  }

  private abstract class Impl[S <: Sys[S]](implicit val undoManager: UndoManager, val workspace: Workspace[S],
                                           val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with FolderView[S] with ModelImpl[FolderView.Update[S]] {
    view =>

    private type Data     = ObjView[S]
    private type NodeView = TreeTableView.NodeView[S, Obj[S], Folder[S], Data]

    protected object TTHandler
      extends TreeTableView.Handler[S, Obj[S], Folder[S], Folder.Update[S], ObjView[S]] {

      def branchOption(node: Obj[S]): Option[Folder[S]] = node.elem match {
        case fe: FolderElem[S] => Some(fe.peer)
        case _ => None
      }

      def children(branch: Folder[S])(implicit tx: S#Tx): lucre.data.Iterator[S#Tx, Obj[S]] =
        branch.iterator

      private def updateObjectName(obj: Obj[S], name: String)(implicit tx: S#Tx): Boolean = {
        treeView.nodeView(obj).exists { nv =>
          val objView = nv.renderData
          deferTx {
            objView.name = name
          }
          true
        }
      }

      private def updateObject(obj: Obj[S], upd: Obj.Update[S])(implicit tx: S#Tx): Boolean =
        (false /: upd.changes) { (p, ch) =>
          val p1 = ch match {
            case Obj.ElemChange(u1) =>
              treeView.nodeView(obj).exists { nv =>
                val objView = nv.renderData
                objView.isUpdateVisible(u1)
              }
            case Obj.AttrAdded  (ObjKeys.attrName, StringElem.Obj(e)) => updateObjectName(obj, e.elem.peer.value)
            case Obj.AttrRemoved(ObjKeys.attrName, _) => updateObjectName(obj, "<unnamed>")
            case Obj.AttrChange (ObjKeys.attrName, _, changes) =>
              (false /: changes) {
                case (res, Obj.ElemChange(Change(_, name: String))) =>
                  res | updateObjectName(obj, name)
                case (res, _) => res
              }
            case _ => false
          }
          p | p1
        }

      private type MUpdate = ModelUpdate[Obj[S], Folder[S]]

      def mapUpdate(update: Update[S])(implicit tx: S#Tx): Vec[MUpdate] =
        updateBranch(treeView.root(), update.changes)

      private def updateBranch(parent: Folder[S], changes: Vec[Folder.Change[S]])(implicit tx: S#Tx): Vec[MUpdate] =
        changes.flatMap {
          case Folder.Added  (idx, obj) => Vec(TreeTableView.NodeAdded  (parent, idx, obj): MUpdate)
          case Folder.Removed(idx, obj) => Vec(TreeTableView.NodeRemoved(parent, idx, obj): MUpdate)
          case Folder.Element(obj, upd) =>
            val isDirty = updateObject(obj, upd)
            val v1: Vec[MUpdate] = obj match {
              case FolderElem.Obj(objT) =>
                upd.changes.flatMap {
                  case Obj.ElemChange(f) => updateBranch(objT.elem.peer, f.asInstanceOf[Folder.Update[S]].changes)
                  case _ => Vec.empty
                }
              case _ => Vec.empty
            }
            if (isDirty) (TreeTableView.NodeChanged(obj): MUpdate) +: v1 else v1
        }

      private lazy val component = TreeTableCellRenderer.Default

      def renderer(tt: TreeTableView[S, Obj[S], Folder[S], Data], node: NodeView, row: Int, column: Int,
                   state: TreeTableCellRenderer.State): Component = {
        val data    = node.renderData
        val value1  = if (column == 0) data.name else "" // data.value
        // val value1  = if (value != {}) value else null
        val res = component.getRendererComponent(tt.treeTable, value1, row = row, column = column, state = state)
        if (column == 0) {
          if (row >= 0 && node.isLeaf) {
            try {
              // val node = t.getNode(row)
              component.icon = data.icon
            } catch {
              case NonFatal(_) => // XXX TODO -- currently NPE problems; seems renderer is called before tree expansion with node missing
            }
          }
          res // component
        } else {
          data.configureRenderer(component)
        }
      }

      private var editView    = Option.empty[ObjView[S]]
      private var editColumn  = 0

      private lazy val defaultEditorJ = new javax.swing.JTextField
      private lazy val defaultEditor: TreeTableCellEditor = {
        val res = new DefaultTreeTableCellEditor(defaultEditorJ)
        res.addCellEditorListener(new CellEditorListener {
          def editingCanceled(e: ChangeEvent) = ()
          def editingStopped (e: ChangeEvent): Unit = editView.foreach { objView =>
            editView = None
            val editOpt: Option[UndoableEdit] = cursor.step { implicit tx =>
              val text = defaultEditorJ.getText
              if (editColumn == 0) {
                val valueOpt: Option[Expr[S, String]] /* Obj[S] */ = if (text.isEmpty || text.toLowerCase == "<unnamed>") None else {
                  val expr = StringEx.newConst[S](text)
                  // Some(Obj(StringElem(elem)))
                  Some(expr)
                }
                // val ed = EditAttrMap[S](s"Rename ${objView.prefix} Element", objView.obj(), ObjKeys.attrName, valueOpt)
                import StringEx.serializer
                val ed = EditAttrMap.expr(s"Rename ${objView.prefix} Element", objView.obj(), ObjKeys.attrName,
                  valueOpt)(StringElem[S](_))
                Some(ed)
              } else {
                objView.tryEdit(text)
              }
            }
            editOpt.foreach(undoManager.add)
          }
        })
        res
      }
      private lazy val defaultEditorC = Component.wrap(defaultEditorJ)

      def isEditable(data: Data, column: Int): Boolean = column == 0 || data.isEditable

      val columnNames = Vec[String]("Name", "Value")

      def editor(tt: TreeTableView[S, Obj[S], Folder[S], Data], node: NodeView, row: Int, column: Int,
                 selected: Boolean): (Component, CellEditor) = {
        val data    = node.renderData
        editView    = Some(data)
        editColumn  = column
        val value   = if (column == 0) data.name else data.value.toString
        defaultEditor.getTreeTableCellEditorComponent(tt.treeTable.peer, value, selected, row, column)
        (defaultEditorC, defaultEditor)
      }

      def data(node: Obj[S])(implicit tx: S#Tx): Data = ObjView(node)
    }

    protected def treeView: TreeTableView[S, Obj[S], Folder[S], ObjView[S]]

    def dispose()(implicit tx: S#Tx): Unit = {
      treeView.dispose()
    }

    def root = treeView.root

    protected def guiInit(): Unit = {
      val t = treeView.treeTable
      t.rootVisible = false

      val tabCM = t.peer.getColumnModel
      tabCM.getColumn(0).setPreferredWidth(176)
      tabCM.getColumn(1).setPreferredWidth(272)

      t.listenTo(t.selection)
      t.reactions += {
        case e: TreeTableSelectionChanged[_, _] =>  // this crappy untyped event doesn't help us at all
          // println(s"selection: $e")
          dispatch(FolderView.SelectionChanged(view, selection))
        // case e => println(s"other: $e")
      }
      t.showsRootHandles  = true
      // t.expandPath(TreeTable.Path(_model.root))
      t.dragEnabled       = true
      t.dropMode          = DropMode.ON_OR_INSERT_ROWS
      t.peer.setTransferHandler(new TransferHandler {
        // ---- export ----

        override def getSourceActions(c: JComponent): Int =
          TransferHandler.COPY | TransferHandler.MOVE | TransferHandler.LINK // dragging only works when MOVE is included. Why?

        override def createTransferable(c: JComponent): Transferable = {
          val sel     = selection
          val trans0  = DragAndDrop.Transferable(FolderView.SelectionFlavor) {
            new FolderView.SelectionDnDData(workspace, sel)
          }
          val trans1 = if (sel.size == 1) {
            val _res = DragAndDrop.Transferable(ObjView.Flavor) {
              new ObjView.Drag(workspace, sel.head.renderData)
            }
            DragAndDrop.Transferable.seq(trans0, _res)
          } else trans0

          trans1
        }

        // ---- import ----
        override def canImport(support: TransferSupport): Boolean =
          treeView.dropLocation match {
            case Some(tdl) =>
              // println(tdl.path)
              // println(s"last = ${tdl.path.lastOption}; column ${tdl.column}") // ; isLeaf? ${t.treeModel.isLeaf(tdl.path.last)}")
              val locOk = tdl.index >= 0 || (tdl.column == 0 && !tdl.path.lastOption.exists(_.isLeaf))
              if (locOk) {
                // println("Supported flavours:")
                // support.getDataFlavors.foreach(println)

                // println(s"File? ${support.isDataFlavorSupported(java.awt.datatransfer.DataFlavor.javaFileListFlavor)}")
                // println(s"Action = ${support.getUserDropAction}")

                support.isDataFlavorSupported(DataFlavor.javaFileListFlavor) ||
                support.isDataFlavorSupported(FolderView.SelectionFlavor   )

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
            case FolderElem.Obj(objT) =>
              objT.elem.peer == newParent || objT.elem.peer.iterator.toList.exists(isNested)
            case _ => false
          }

          val isMove  = dropAction == TransferHandler.MOVE
          val isCopy  = dropAction == TransferHandler.COPY

          // make sure we are not moving a folder within itself (it will magically disappear :)
          val sel1 = if (!isMove) sel else sel.filterNot(nv => isNested(nv.modelData()))

          // if we move children within the same folder, adjust the insertion index by
          // decrementing it for any child which is above the insertion index, because
          // we will first remove all children, then re-insert them.
          val idx0 = if (idx >= 0) idx else newParent /* .children */.size
          val idx1 = if (!isMove) idx0 else idx0 - sel1.count { nv =>
            val isInNewParent = nv.parent == newParent
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
              val edit = EditFolderRemoveObj[S](nv.renderData.prefix, parent, idx, childH())
              Some(edit)
            }
          }

          val editInsert = sel1.zipWithIndex.map { case (nv, off) =>
            val childH  = nv.modelData
            val child0  = childH()
            val child   = if (!isCopy) child0 else Obj.copy(child0)
            EditFolderInsertObj[S](nv.renderData.prefix, newParent, idx1 + off, child)
          }
          val edits: List[UndoableEdit] = editRemove ++ editInsert
          val name = sel1 match {
            case single :: Nil  => single.renderData.prefix
            case _              => "Elements"
          }

          val prefix = if (isMove) "Move" else if (isCopy) "Copy" else "Link"
          CompoundEdit(edits, s"$prefix $name")
        }

        private def importSelection(support: TransferSupport, parent: Folder[S], index: Int)
                                   (implicit tx: S#Tx): Option[UndoableEdit] = {
          val data = support.getTransferable.getTransferData(FolderView.SelectionFlavor)
            .asInstanceOf[FolderView.SelectionDnDData[S]]
          if (data.workspace == workspace) {
            val sel     = data.selection
            insertData(sel, parent, idx = index, dropAction = support.getDropAction)
          } else {
            None
          }
        }

        private def importFiles(support: TransferSupport, parent: Folder[S], index: Int)
                               (implicit tx: S#Tx): Option[UndoableEdit] = {
          import JavaConversions._
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

        override def importData(support: TransferSupport): Boolean =
          treeView.dropLocation.exists { tdl =>
            val editOpt = cursor.step { implicit tx =>
              val parentOpt = tdl.path.lastOption.fold(Option(treeView.root())) { nodeView =>
                nodeView.modelData() match {
                  case FolderElem.Obj(objT) => Some(objT.elem.peer)
                  case _ => None
                }
              }
              parentOpt.flatMap { parent =>
                val idx = tdl.index
                if (support.isDataFlavorSupported(FolderView.SelectionFlavor))
                  importSelection(support, parent, idx)
                else if (support.isDataFlavorSupported(DataFlavor.javaFileListFlavor))
                  importFiles(support, parent, idx)
                else None
              }
            }
            editOpt.foreach(undoManager.add)
            editOpt.isDefined
          }
      })

      component     = treeView.component
    }

    def selection: FolderView.Selection[S] = treeView.selection

    def insertionPoint(implicit tx: S#Tx): (Folder[S], Int) = treeView.insertionPoint

    def locations: Vec[ObjView.ArtifactLocation[S]] = selection.flatMap { nodeView =>
      nodeView.renderData match {
        case view: ObjView.ArtifactLocation[S] => Some(view)
        case _ => None
      }
    } (breakOut)

    def findLocation(f: File): Option[ActionArtifactLocation.QueryResult[S]] = {
      val locationsOk = locations.flatMap { view =>
        try {
          Artifact.relativize(view.directory, f)
          Some(view)
        } catch {
          case NonFatal(_) => None
        }
      } .headOption

      locationsOk match {
        case Some(loc)  => Some(Left(loc.obj))
        case _          =>
          val parent = selection.flatMap { nodeView =>
            nodeView.renderData match {
              case f: ObjView.Folder[S] => Some(f.obj)
              case _ => None
            }
          } .headOption
          ActionArtifactLocation.query[S](treeView.root, file = f /*, folder = parent */) // , window = Some(comp))
      }
    }
  }
}