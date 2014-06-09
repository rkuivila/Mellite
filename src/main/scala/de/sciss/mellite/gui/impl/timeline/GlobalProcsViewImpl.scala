/*
 *  GlobalProcsViewImpl.scala
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
package timeline

import de.sciss.lucre.stm
import de.sciss.synth.proc.ProcGroup
import scala.swing.{Action, Swing, BorderPanel, FlowPanel, ScrollPane, Button, Table, Component}
import scala.collection.immutable.{IndexedSeq => Vec}
import javax.swing.table.{TableColumnModel, AbstractTableModel}
import scala.annotation.switch
import Swing._
import de.sciss.desktop.OptionPane
import javax.swing.{JTable, JComponent, TransferHandler, DropMode}
import javax.swing.TransferHandler.TransferSupport
import java.awt.datatransfer.Transferable
import scala.swing.event.TableColumnsSelected
import scala.util.Try
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.{Int => IntEx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing._
import de.sciss.file.File
import de.sciss.icons.raphael

object GlobalProcsViewImpl {
  def apply[S <: Sys[S]](document: File /* Document[S] */, group: ProcGroup[S], selectionModel: ProcSelectionModel[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): GlobalProcsView[S] = {

    import ProcGroup.Modifiable.serializer
    val groupHOpt = group.modifiableOption.map(tx.newHandle(_))
    val view      = new Impl[S](document, groupHOpt, selectionModel)
    deferTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](document: File /* Document[S] */,
                                        groupHOpt: Option[stm.Source[S#Tx, ProcGroup.Modifiable[S]]],
                                        selectionModel: ProcSelectionModel[S])
                                       (implicit cursor: stm.Cursor[S])
    extends GlobalProcsView[S] with ComponentHolder[Component] {

    private var procSeq = Vec.empty[ProcView[S]]

    private def atomic[A](block: S#Tx => A): A = cursor.step(block)

    private var table: Table = _

    private val selectionListener: ProcSelectionModel.Listener[S] = {
      case ProcSelectionModel.Update(_, _) =>
        val items = selectionModel.iterator.flatMap { pv =>
          pv.outputs.flatMap {
            case (_, links) =>
              links.flatMap { link =>
                val tgt = link.target
                if (tgt.isGlobal) Some(tgt) else None
              }
          }
        } .toSet

        val indices   = items.map(procSeq.indexOf(_))
        val rows      = table.selection.rows
        val toAdd     = indices.filterNot(rows.contains)
        val toRemove  = rows.filterNot(indices.contains)

        if (toRemove.nonEmpty) rows --= toRemove
        if (toAdd   .nonEmpty) rows ++= toAdd
    }

    // columns: name, gain, muted, bus
    private val tm = new AbstractTableModel {
      def getRowCount     = procSeq.size
      def getColumnCount  = 4

      def getValueAt(row: Int, col: Int): AnyRef = {
        val pv  = procSeq(row)
        val res = (col: @switch) match {
          case 0 => pv.name
          case 1 => pv.gain
          case 2 => pv.muted
          case 3 => pv.busOption.getOrElse(0)
        }
        res.asInstanceOf[AnyRef]
      }

      override def isCellEditable(row: Int, col: Int): Boolean = true

      override def setValueAt(value: Any, row: Int, col: Int): Unit = {
        val pv = procSeq(row)
        (col, value) match {
          case (0, name: String) =>
            atomic { implicit tx =>
              ProcActions.rename(pv.proc, if (name.isEmpty) None else Some(name))
            }
          case (1, gainS: String) =>  // XXX TODO: should use spinner for editing
            Try(gainS.toDouble).foreach { gain =>
              atomic { implicit tx =>
                ProcActions.setGain(pv.proc, gain)
              }
            }

          case (2, muted: Boolean) =>
            atomic { implicit tx =>
              ProcActions.toggleMute(pv.proc)
            }

          case (3, busS: String) =>   // XXX TODO: should use spinner for editing
            Try(busS.toInt).foreach { bus =>
              atomic { implicit tx =>
                ProcActions.setBus(pv.proc :: Nil, IntEx.newConst(bus))
              }
            }

          case _ =>
        }
      }

      override def getColumnName(col: Int): String = (col: @switch) match {
        case 0 => "Name"
        case 1 => "Gain"
        case 2 => "M" // short because column only uses checkbox
        case 3 => "Bus"
        // case other => super.getColumnName(col)
      }

      override def getColumnClass(col: Int): Class[_] = (col: @switch) match {
        case 0 => classOf[String]
        case 1 => classOf[Double]
        case 2 => classOf[Boolean]
        case 3 => classOf[Int]
      }
    }

    private def addItemWithDialog(): Unit =
      groupHOpt.foreach { groupH =>
        val op    = OptionPane.textInput(message = "Name:", initial = "Bus")
        op.title  = "Add Global Proc"
        op.show(None).foreach { name =>
          // println(s"Add proc: $name")
          atomic { implicit tx =>
            ProcActions.insertGlobalRegion(groupH(), name, bus = None)
          }
        }
      }

    private def removeProcs(pvs: Iterable[ProcView[S]]): Unit =
      if (pvs.nonEmpty) groupHOpt.foreach { groupH =>
        atomic { implicit tx =>
          ProcGUIActions.removeProcs(groupH(), pvs)
        }
      }

    private def setColumnWidth(tcm: TableColumnModel, idx: Int, w: Int): Unit = {
      val tc = tcm.getColumn(idx)
      tc.setPreferredWidth(w)
      // tc.setMaxWidth      (w)
    }

    def guiInit(): Unit = {
      table             = new Table()
      // XXX TODO: enable the following - but we're loosing default boolean rendering
      //        // Table default has idiotic renderer/editor handling
      //        override lazy val peer: JTable = new JTable /* with Table.JTableMixin */ with SuperMixin
      //      }
      table.model       = tm
      // table.background  = Color.darkGray
      val jt            = table.peer
      jt.setAutoCreateRowSorter(true)
      // jt.putClientProperty("JComponent.sizeVariant", "small")
      // jt.getRowSorter.setSortKeys(...)
      //      val tcm = new DefaultTableColumnModel {
      //
      //      }
      val tcm = jt.getColumnModel
      setColumnWidth(tcm, 0, 55)
      setColumnWidth(tcm, 1, 47)
      setColumnWidth(tcm, 2, 29)
      setColumnWidth(tcm, 3, 43)
      jt.setPreferredScrollableViewportSize(177 -> 100)

      // ---- drag and drop ----
      jt.setDropMode(DropMode.ON)
      jt.setDragEnabled(true)
      jt.setTransferHandler(new TransferHandler {
        override def getSourceActions(c: JComponent): Int = TransferHandler.LINK

        override def createTransferable(c: JComponent): Transferable = {
          val sel = table.selection.rows
          sel.headOption.map { row =>
            val pv = procSeq(row)
            DragAndDrop.Transferable(timeline.DnD.flavor)(timeline.DnD.ProcDrag(document, pv.procSource))
          } .orNull
        }

        // ---- import ----
        override def canImport(support: TransferSupport): Boolean =
          support.isDataFlavorSupported(timeline.DnD.flavor)

        override def importData(support: TransferSupport): Boolean =
          support.isDataFlavorSupported(timeline.DnD.flavor) && {
            Option(jt.getDropLocation).fold(false) { dl =>
              val pv    = procSeq(dl.getRow)
              val drag  = support.getTransferable.getTransferData(timeline.DnD.flavor).asInstanceOf[timeline.DnD.Drag[S]]
              drag match {
                case timeline.DnD.IntDrag (`document`, source) =>
                  atomic { implicit tx =>
                    val intExpr = source().elem.peer
                    ProcActions.setBus(pv.proc :: Nil, intExpr)
                  }
                  true

                case timeline.DnD.CodeDrag(`document`, source) =>
                  atomic { implicit tx =>
                    val codeElem = source()
                    ProcActions.setSynthGraph(pv.proc :: Nil, codeElem)
                  }

                case _ => false
              }
            }
          }
      })

      val scroll    = new ScrollPane(table)
      scroll.border = null

      val actionAdd = Action(null)(addItemWithDialog())
      val ggAdd: Button = GUI.toolButton(actionAdd, raphael.Shapes.Plus, "Add Global Process")
      // ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      val actionDelete = Action(null) {
        val pvs = table.selection.rows.map(procSeq)
        removeProcs(pvs)
      }
      val ggDelete: Button = GUI.toolButton(actionDelete, raphael.Shapes.Minus, "Delete Global Process")
      actionDelete.enabled = false
      // ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")
      table.listenTo(table.selection)
      table.reactions += {
        case TableColumnsSelected(_, range, _) => actionDelete.enabled = range.nonEmpty
      }

      val butPanel  = new FlowPanel(ggAdd, ggDelete)

      selectionModel addListener selectionListener

      component = new BorderPanel {
        add(scroll, BorderPanel.Position.Center)
        if (groupHOpt.isDefined) add(butPanel, BorderPanel.Position.South) // only add buttons if group is modifiable
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = deferTx {
      selectionModel removeListener  selectionListener
    }

    def add(proc: ProcView[S]): Unit = {
      val row   = procSeq.size
      procSeq :+= proc
      tm.fireTableRowsInserted(row, row)
    }

    def remove(proc: ProcView[S]): Unit = {
      val row   = procSeq.indexOf(proc)
      procSeq   = procSeq.patch(row, Vec.empty, 1)
      tm.fireTableRowsDeleted(row, row)
    }

    def updated(proc: ProcView[S]): Unit = {
      val row   = procSeq.indexOf(proc)
      tm.fireTableRowsUpdated(row, row)
    }
  }
}