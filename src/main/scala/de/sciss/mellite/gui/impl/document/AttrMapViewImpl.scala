/*
 *  AttrMapViewImpl.scala
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

import javax.swing.TransferHandler.TransferSupport

import de.sciss.swingplus.DropMode
import de.sciss.synth.proc.{StringElem, ProcKeys, Obj}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.impl.ComponentHolder
import scala.swing.{TextField, Label, Swing, ScrollPane, Table}
import de.sciss.lucre.swing.deferTx
import de.sciss.desktop.{OptionPane, UndoManager}
import de.sciss.lucre.synth.Sys
import javax.swing.table.{TableCellEditor, DefaultTableCellRenderer, AbstractTableModel}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.annotation.switch
import de.sciss.lucre.stm.Disposable
import scala.concurrent.stm.TMap
import de.sciss.model.Change
import scala.swing.event.TableColumnsSelected
import de.sciss.model.impl.ModelImpl
import Swing._
import javax.swing.{TransferHandler, AbstractCellEditor, JTable}
import de.sciss.mellite.gui.edit.{CompoundEdit, EditAttrMap}
import java.util.EventObject
import java.awt.event.MouseEvent

object AttrMapViewImpl {
  def apply[S <: Sys[S]](workspace: Workspace[S], obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      undoManager: UndoManager): AttrMapView[S] = {
    val map   = obj.attr
    val objH  = tx.newHandle(obj)

    val list0 = map.iterator.map {
      case (key, value) =>
        val view = ObjView(value)
        (key, view)
    } .toIndexedSeq

    val res = new Impl(workspace, objH, list0) {
      val observer = obj.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          case Obj.AttrAdded  (key, value) =>
            val view = ObjView(value)
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

  // private final class EntryView[S <: Sys[S]](var name: String, val obj: ObjView[S])

  private abstract class Impl[S <: Sys[S]](val workspace: Workspace[S], mapH: stm.Source[S#Tx, Obj[S]],
                                           list0: Vec[(String, ObjView[S])])(implicit val cursor: stm.Cursor[S],
                                           val undoManager: UndoManager)
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

    final protected def attrAdded(key: String, view: ObjView[S])(implicit tx: S#Tx): Unit = {
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
        val row = model.indexWhere(_._1 == key)
        if (row < 0) {
          warnNoView(key)
        } else {
          model = model.patch(row, Nil, 1)
          tableModel.fireTableRowsDeleted(row, row)
        }
      }
    }

    private def warnNoView(key: String): Unit = println(s"Warning: AttrMapView - no view found for $key")

    private def updateObjectName(objView: ObjView[S], name: String)(implicit tx: S#Tx): Unit =
      deferTx { objView.name = name }

    private def updateObject(objView: ObjView[S], changes: Vec[Obj.Change[S, Any]])
                            (implicit tx: S#Tx): Boolean =
      (false /: changes) { (p, ch) =>
        val p1 = ch match {
          case Obj.ElemChange(u1) =>
            objView.isUpdateVisible(u1)
          case Obj.AttrAdded  (ProcKeys.attrName, e: StringElem[S]) =>
            updateObjectName(objView, e.peer.value)
            true
          case Obj.AttrRemoved(ProcKeys.attrName, _) =>
            updateObjectName(objView, "<unnamed>")
            true
          case Obj.AttrChange (ProcKeys.attrName, _, nameChanges) =>
            (false /: nameChanges) {
              case (_, Obj.ElemChange(Change(_, name: String))) =>
                updateObjectName(objView, name)
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
          val row = model.indexWhere(_._1 == key)
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
            wrap.text = if (view.name == "<unnamed>") "" else view.name
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
            case view: ObjView[_] => view.configureRenderer(wrap).peer
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

        def getTableCellEditorComponent(table: JTable, value: Any, isSelected: Boolean, row: Int,
                                        col: Int): java.awt.Component = {
          // println("AQUI")
          val view      = model(row)._2
          // currentValue  = view.value
          editor.text   = view.value.toString
          editor.peer
        }
      })
      GUI.sortTable(tab, 0)

      jt.setDragEnabled(true)
      jt.setDropMode(DropMode.OnOrInsertRows)
      jt.setTransferHandler(new TransferHandler {
        override def canImport(support: TransferSupport): Boolean = {
          val res = support.isDrop && {
            val dl = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            val locOk = dl.isInsertRow || {
              val viewCol   = dl.getColumn
              val modelCol  = jt.convertColumnIndexToModel(viewCol)
              modelCol >= 1   // should drop on the 'type' or 'value' column
            }
            // println(s"locOk? $locOk")
            locOk && support.isDataFlavorSupported(ObjView.Flavor)
          }
          res
        }

        override def importData(support: TransferSupport): Boolean = {
          val res = support.isDrop && {
            val dl        = support.getDropLocation.asInstanceOf[JTable.DropLocation]
            val isInsert  = dl.isInsertRow
            val view      = support.getTransferable.getTransferData(ObjView.Flavor).asInstanceOf[ObjView.Drag[S]].view
            val keyOpt = if (isInsert) {   // ---- create new entry with key via dialog ----
              // XXX TODO: initial key could use sensible default depending on value type
              val opt   = OptionPane.textInput(message = "Key Name", initial = "key")
              opt.title = "Create Attribute"
              opt.show(GUI.findWindow(component))

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
        case TableColumnsSelected(_, _, _) => // note: range is range of _changes_ rows, not current selection
          val indices = tab.selection.rows /* range */.toList.sorted
          // println(s"indices = $indices")
          val sel     = indices.map(model.apply)
          dispatch(AttrMapView.SelectionChanged(impl, sel))
      }
    }

    final def selection: List[(String, ObjView[S])] =
      tab.selection.rows.toList.sorted.map(model.apply)
  }
}
