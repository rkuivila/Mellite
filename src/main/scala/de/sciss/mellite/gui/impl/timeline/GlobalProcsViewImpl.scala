/*
 *  GlobalProcsViewImpl.scala
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
package timeline

import de.sciss.lucre.stm
import de.sciss.synth.proc.ProcGroup
import scala.swing.{Swing, BorderPanel, FlowPanel, ScrollPane, Button, Table, Component}
import collection.immutable.{IndexedSeq => Vec}
import javax.swing.table.{TableColumnModel, AbstractTableModel}
import scala.annotation.switch
import Swing._
import de.sciss.desktop.OptionPane
import javax.swing.{JComponent, TransferHandler, DropMode}
import javax.swing.TransferHandler.TransferSupport
import java.awt.datatransfer.Transferable
import scala.swing.event.TableColumnsSelected
import scala.util.Try
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.synth.expr.Ints

object GlobalProcsViewImpl {
  def apply[S <: Sys[S]](document: Document[S], group: ProcGroup[S], selectionModel: ProcSelectionModel[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): GlobalProcsView[S] = {

    import ProcGroup.Modifiable.serializer
    val groupHOpt = group.modifiableOption.map(tx.newHandle(_))
    val view      = new Impl[S](document, groupHOpt, selectionModel)
    guiFromTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](document: Document[S],
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

      def getValueAt(row: Int, column: Int): AnyRef = {
        val pv  = procSeq(row)
        val res = (column: @switch) match {
          case 0 => pv.name
          case 1 => pv.gain
          case 2 => pv.muted
          case 3 => pv.busOption.getOrElse(0)
        }
        res.asInstanceOf[AnyRef]
      }

      override def isCellEditable(row: Int, column: Int): Boolean = true

      override def setValueAt(value: Any, row: Int, column: Int): Unit = {
        val pv = procSeq(row)
        (column, value) match {
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
                ProcActions.setBus(pv.proc :: Nil, Ints.newConst(bus))
              }
            }

          case _ =>
        }
      }

      override def getColumnName(column: Int): String = (column: @switch) match {
        case 0 => "Name"
        case 1 => "Gain"
        case 2 => "M" // short because column only uses checkbox
        case 3 => "Bus"
        case other => super.getColumnName(column)
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
      table.peer.putClientProperty("JComponent.sizeVariant", "small")
      table.model       = tm
      // table.background  = Color.darkGray
      val jt            = table.peer
      jt.setAutoCreateRowSorter(true)
      //      val tcm = new DefaultTableColumnModel {
      //
      //      }
      val tcm = jt.getColumnModel
      setColumnWidth(tcm, 0, 48)
      setColumnWidth(tcm, 1, 32)
      setColumnWidth(tcm, 2, 16)
      setColumnWidth(tcm, 3, 24)
      val tj = table.peer
      tj.setPreferredScrollableViewportSize(124 -> 100)

      // ---- drag and drop ----
      tj.setDropMode(DropMode.ON)
      tj.setDragEnabled(true)
      tj.setTransferHandler(new TransferHandler {
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
            Option(tj.getDropLocation).fold(false) { dl =>
              val pv    = procSeq(dl.getRow)
              val drag  = support.getTransferable.getTransferData(timeline.DnD.flavor).asInstanceOf[timeline.DnD.Drag[S]]
              drag match {
                case timeline.DnD.IntDrag (`document`, source) =>
                  atomic { implicit tx =>
                    val intExpr = source().entity
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

      val ggAdd = Button("+")(addItemWithDialog())
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      val ggDelete: Button = Button("\u2212") {
        val pvs = table.selection.rows.map(procSeq)
        removeProcs(pvs)
      }
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")
      table.listenTo(table.selection)
      table.reactions += {
        case TableColumnsSelected(_, range, _) => ggDelete.enabled = range.nonEmpty
      }

      val butPanel  = new FlowPanel(ggAdd, ggDelete)

      selectionModel addListener selectionListener

      comp          = new BorderPanel {
        add(scroll, BorderPanel.Position.Center)
        if (groupHOpt.isDefined) add(butPanel, BorderPanel.Position.South) // only add buttons if group is modifiable
      }
    }

    def dispose()(implicit tx: S#Tx): Unit = guiFromTx {
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