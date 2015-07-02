/*
 *  AttrMapViewImpl.scala
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

import java.awt.datatransfer.Transferable
import java.awt.event.MouseEvent
import java.util.EventObject
import javax.swing.TransferHandler.TransferSupport
import javax.swing.table.{AbstractTableModel, DefaultTableCellRenderer, TableCellEditor}
import javax.swing.{AbstractCellEditor, JComponent, JTable, TransferHandler}

import de.sciss.desktop.edit.CompoundEdit
import de.sciss.desktop.{Window, OptionPane, UndoManager}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.EditAttrMap
import de.sciss.model.Change
import de.sciss.model.impl.ModelImpl
import de.sciss.swingplus.DropMode
import de.sciss.synth.proc.{Obj, ObjKeys, StringElem}
import org.scalautils.TypeCheckedTripleEquals

import scala.annotation.switch
import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.stm.TMap
import scala.swing.Swing._
import scala.swing.event.TableRowsSelected
import scala.swing.{Label, ScrollPane, Table, TextField}

object AttrMapViewImpl {
  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      workspace: Workspace[S], undoManager: UndoManager): AttrMapView[S] = {
    val map   = obj.attr
    val objH  = tx.newHandle(obj)

    val list0 = map.iterator.map {
      case (key, value) =>
        val view = ListObjView(value)
        (key, view)
    } .toIndexedSeq

    val res = new Impl(objH, list0) {
      val observer = obj.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.AttrAdded  (key, value) =>
            val view = ListObjView(value)
            attrAdded(key, view)
          case Obj.AttrRemoved(key, value) =>
            attrRemoved(key)
          case Obj.AttrChange (key, value, changes) =>
            attrChange(key, value, changes)
          case _ =>
        }
      }

      deferTx {
        guiInit()
      }
    }

    res
  }

  private abstract class Impl[S <: Sys[S]](mapH: stm.Source[S#Tx, Obj[S]],
                                           list0: Vec[(String, ListObjView[S])])(implicit val cursor: stm.Cursor[S],
                                           val workspace: Workspace[S], val undoManager: UndoManager)
    extends AttrMapView[S] with ComponentHolder[ScrollPane] with ModelImpl[AttrMapView.Update[S]] {
    impl =>

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      // viewMap.clear()
    }

    protected def observer: Disposable[S#Tx]

    private var model = list0

    private var tab: Table = _

    private val viewMap = TMap(list0: _*)

    final def obj(implicit tx: S#Tx): Obj[S] = mapH()

    final protected def attrAdded(key: String, view: ListObjView[S])(implicit tx: S#Tx): Unit = {
      viewMap.+=(key -> view)(tx.peer)
      deferTx {
        val row = model.size
        model :+= key -> view
        tableModel.fireTableRowsInserted(row, row)
      }
    }

    final protected def attrRemoved(key: String)(implicit tx: S#Tx): Unit = {
      viewMap.-=(key)(tx.peer)
      deferTx {
        import TypeCheckedTripleEquals._
        val row = model.indexWhere(_._1 === key)
        if (row < 0) {
          warnNoView(key)
        } else {
          model = model.patch(row, Nil, 1)
          tableModel.fireTableRowsDeleted(row, row)
        }
      }
    }

    private def warnNoView(key: String): Unit = println(s"Warning: AttrMapView - no view found for $key")

    private def updateObjectName(objView: ObjView[S], nameOption: Option[String])(implicit tx: S#Tx): Unit =
      deferTx { objView.nameOption = nameOption }

    private def updateObject(objView: ListObjView[S], changes: Vec[Obj.Change[S, Any]])
                            (implicit tx: S#Tx): Boolean =
      (false /: changes) { (p, ch) =>
        val p1 = ch match {
          case Obj.ElemChange(u1) =>
            objView.isUpdateVisible(u1)
          case Obj.AttrAdded  (ObjKeys.attrName, e: StringElem[S]) =>
            updateObjectName(objView, Some(e.peer.value))
            true
          case Obj.AttrRemoved(ObjKeys.attrName, _) =>
            updateObjectName(objView, None)
            true
          case Obj.AttrChange (ObjKeys.attrName, _, nameChanges) =>
            (false /: nameChanges) {
              case (_, Obj.ElemChange(Change(_, name: String))) =>
                updateObjectName(objView, Some(name))
                true
              case (res, _) => res
            }
          case _ => false
        }
        p | p1
      }

    final protected def attrChange(key: String, value: Obj[S], changes: Vec[Obj.Change[S, Any]])
                                  (implicit tx: S#Tx): Unit = {
      val viewOpt = viewMap.get(key)(tx.peer)
      viewOpt.fold {
        warnNoView(key)
      } { view =>
        val isDirty = updateObject(view, changes)
        if (isDirty) deferTx {
          import TypeCheckedTripleEquals._
          val row = model.indexWhere(_._1 === key)
          if (row < 0) {
            warnNoView(key)
          } else {
            tableModel.fireTableRowsUpdated(row, row)
          }
        }
      }
    }

    private object tableModel extends AbstractTableModel {
      def getRowCount   : Int = model.size
      def getColumnCount: Int = 3

      override def getColumnName(col: Int): String = (col: @switch) match {
        case 0 => "Key"
        case 1 => "Name"
        case 2 => "Value"
      }

      def getValueAt(row /* rowView */: Int, col: Int): AnyRef = {
        // val row = tab.peer.convertRowIndexToModel(rowView)
        (col: @switch) match {
          case 0 => model(row)._1
          case 1 => model(row)._2 // .name
          case 2 => model(row)._2 // .value.asInstanceOf[AnyRef]
        }
      }

      override def getColumnClass(col: Int): Class[_] = (col: @switch) match {
        case 0 => classOf[String]
        case 1 => classOf[ObjView[S]]
        case 2 => classOf[ObjView[S]]
      }

      override def isCellEditable(row: Int, col: Int): Boolean = {
        val res = col == 0 || model(row)._2.isEditable
        // println(s"isCellEditable(row = $row, col = $col) -> $res")
        res
      }

      // println(s"setValueAt(value = $value, row = $row, col = $col")
      override def setValueAt(editValue: Any, row: Int, col: Int): Unit = (col: @switch) match {
        case 0 =>
          val (oldKey, view) = model(row)
          val newKey  = editValue.toString
          if (oldKey != newKey) {
            val editOpt = cursor.step { implicit tx =>
              val value = view.obj()
              val obj0  = obj
              val ed1   = EditAttrMap(name = "Remove", obj0, key = oldKey, value = None)
              val ed2   = EditAttrMap(name = "Insert", obj0, key = newKey, value = Some(value))
              CompoundEdit(ed1 :: ed2 :: Nil, s"Rename Attribute Key")
            }
            editOpt.foreach(undoManager.add)
          }

        case 2 =>
          val view    = model(row)._2
          val editOpt = cursor.step { implicit tx => view.tryEdit(editValue) }
          editOpt.foreach(undoManager.add)

        case _ =>
      }
    }

    final protected def guiInit(): Unit = {
      tab = new Table {
        // Table default has idiotic renderer/editor handling
        override lazy val peer: JTable = new JTable /* with Table.JTableMixin */ with SuperMixin
      }
      tab.model     = tableModel
      val jt        = tab.peer
      jt.setAutoCreateRowSorter(true)
      val tcm       = jt.getColumnModel
      val colName   = tcm.getColumn(0)
      val colTpe    = tcm.getColumn(1)
      val colValue  = tcm.getColumn(2)
      colName .setPreferredWidth( 96)
      colTpe  .setPreferredWidth( 96)
      colValue.setPreferredWidth(208)
      jt.setPreferredScrollableViewportSize(390 -> 160)
      // val colName = tcm.getColumn(0)
      //      colName.setCellEditor(new DefaultTreeTableCellEditor() {
      //        override def stopCellEditing(): Boolean = super.stopCellEditing()
      //      })
      colTpe.setCellRenderer(new DefaultTableCellRenderer {
        outer =>
        private val wrap = new Label { override lazy val peer = outer }

        override def setValue(value: Any): Unit = value match {
          case view: ObjView[_] =>
            import TypeCheckedTripleEquals._
            wrap.text = if (view.name === "<unnamed>") "" else view.name
            wrap.icon = view.icon
          case _ =>
        }

        //        override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean,
        //                                                   hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
        //          super.getTableCellRendererComponent(table, null, isSelected, hasFocus, row, column)
        //          value match {
        //            case view: ObjView[_] =>
        //              wrap.text = view.name
        //              wrap.icon = view.icon
        //            case _ =>
        //          }
        //          outer
        //        }
      })
      colValue.setCellRenderer(new DefaultTableCellRenderer {
        outer =>
        private val wrap = new Label { override lazy val peer = outer }
        override def getTableCellRendererComponent(table: JTable, value: Any, isSelected: Boolean,
                                                   hasFocus: Boolean, row: Int, column: Int): java.awt.Component = {
          super.getTableCellRendererComponent(table, null, isSelected, hasFocus, row, column)
          value match {
            case view: ListObjView[_] => view.configureRenderer(wrap).peer
            case _ => outer
          }
        }
      })
      colValue.setCellEditor(new AbstractCellEditor with TableCellEditor {
        // private var currentValue: Any = null
        private val editor = new TextField(10)

        override def isCellEditable(e: EventObject): Boolean = e match {
          case m: MouseEvent => m.getClickCount >= 2
          case _ => true
        }

        def getCellEditorValue: AnyRef = editor.text // currentValue.asInstanceOf[AnyRef]

        def getTableCellEditorComponent(table: JTable, value: Any, isSelected: Boolean, rowV: Int,
                                        col: Int): java.awt.Component = {
          val row = table.convertRowIndexToModel(rowV)
          val view      = model(row)._2
          // currentValue  = view.value
          editor.text   = view.value.toString
          editor.peer
        }
      })
      import de.sciss.swingplus.Implicits._
      tab.sort(0)

      jt.setDragEnabled(true)
      jt.setDropMode(DropMode.OnOrInsertRows)
      jt.setTransferHandler(new TransferHandler {
        override def getSourceActions(c: JComponent): Int = TransferHandler.LINK

        override def createTransferable(c: JComponent): Transferable = {
          val sel     = selection
          val trans1 = if (sel.size == 1) {
            val _res = DragAndDrop.Transferable(ListObjView.Flavor) {
              new ListObjView.Drag(workspace, sel.head._2)
            }
            _res
          } else null

          trans1
        }

        override def canImport(support: TransferSupport): Boolean = {
          val res = support.isDrop && {
            val dl = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            val locOk = dl.isInsertRow || {
              val viewCol   = dl.getColumn
              val modelCol  = jt.convertColumnIndexToModel(viewCol)
              modelCol >= 1   // should drop on the 'type' or 'value' column
            }
            // println(s"locOk? $locOk")
            val allOk = locOk && support.isDataFlavorSupported(ListObjView.Flavor)
            if (allOk) support.setDropAction(TransferHandler.LINK)
            allOk
          }
          res
        }

        override def importData(support: TransferSupport): Boolean = {
          val res = support.isDrop && {
            val dl        = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            val isInsert  = dl.isInsertRow
            val view      = support.getTransferable.getTransferData(ListObjView.Flavor).asInstanceOf[ListObjView.Drag[S]].view
            val keyOpt = if (isInsert) { // ---- create new entry with key via dialog ----
              queryKey()
            } else {          // ---- update value of existing entrywith key via dialog ----
              val rowV  = dl.getRow
              val row   = jt.convertRowIndexToModel(rowV)
              Some(model(row)._1)
            }
            // println(s"TODO: ${if (isInsert) "insert" else "replace"} $view")

            keyOpt.exists { key =>
              val edit = cursor.step { implicit tx =>
                val editName = if (isInsert) s"Create Attribute '$key'" else s"Change Attribute '$key'"
                EditAttrMap(name = editName, obj = obj, key = key, value = Some(view.obj()))
              }
              undoManager.add(edit)
              true
            }
          }
          res
        }
      })

      val scroll    = new ScrollPane(tab)
      scroll.border = null
      component     = scroll
      tab.listenTo(tab.selection)
      tab.reactions += {
        case TableRowsSelected(_, _, false) => // note: range is range of _changes_ rows, not current selection
          val sel = selection
          // println(sel.map(_._1))
          dispatch(AttrMapView.SelectionChanged(impl, sel))
      }
    }

    final def queryKey(initial: String): Option[String] = {
      val opt   = OptionPane.textInput(message = "Key Name", initial = initial)
      opt.title = "Create Attribute"
      opt.show(Window.find(component))
    }

    final def selection: List[(String, ObjView[S])] = {
      val ind0: List[Int] = tab.selection.rows.map(tab.peer.convertRowIndexToModel)(breakOut)
      val indices = ind0.sorted
      indices.map(model.apply)
    }
  }
}
