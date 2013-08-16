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
import de.sciss.synth.proc.{Proc, Sys, ProcGroup}
import scala.swing.{Swing, BorderPanel, FlowPanel, ScrollPane, Button, Table, Component}
import collection.immutable.{IndexedSeq => Vec}
import javax.swing.table.{TableColumnModel, AbstractTableModel}
import scala.annotation.switch
import Swing._
import de.sciss.desktop.OptionPane

object GlobalProcsViewImpl {
  def apply[S <: Sys[S]](document: Document[S], group: ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): GlobalProcsView[S] = {

    val view = new Impl[S]
    guiFromTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]](implicit cursor: stm.Cursor[S]) extends GlobalProcsView[S] with ComponentHolder[Component] {

    private var procSeq = Vec.empty[ProcView[S]]

    private val tm = new AbstractTableModel {
      def getRowCount     = procSeq.size
      def getColumnCount  = 4 // currently: name, gain, mute and bus

      def getValueAt(row: Int, column: Int): AnyRef = {
        val pv  = procSeq(row)
        val res = (column: @switch) match {
          case 0 => pv.name
          case 1 => 1.0   // gain: XXX TODO
          case 2 => pv.muted
          case 3 => 0     // bus: XXX TODO
        }
        res.asInstanceOf[AnyRef]
      }
    }

    private def addItemWithDialog(): Unit = {
      // val nameOpt = GUI.keyValueDialog(value = ggValue, title = s"New $tpe", defaultName = tpe)
      val op    = OptionPane.textInput(message = "Name:", initial = "Bus")
      op.title  = "Add Global Proc"
      op.show(None).foreach { name =>
        // println(s"Add proc: $name")

        cursor.step { implicit tx =>
          val proc = Proc[S]
          ???
        }
      }
    }

    private def removeSelectedItem(): Unit = {
      println("TODO: removeSelectedItem")
    }

    private def setColumnWidth(tcm: TableColumnModel, idx: Int, w: Int): Unit = {
      val tc = tcm.getColumn(idx)
      tc.setPreferredWidth(w)
      // tc.setMaxWidth      (w)
    }

    def guiInit(): Unit = {
      val table         = new Table()
      table.model       = tm
      // table.background  = Color.darkGray
      val jt            = table.peer
      jt.setAutoCreateRowSorter(true)
      //      val tcm = new DefaultTableColumnModel {
      //
      //      }
      val tcm = jt.getColumnModel
      setColumnWidth(tcm, 0, 48)
      setColumnWidth(tcm, 1, 24)
      setColumnWidth(tcm, 2, 16)
      setColumnWidth(tcm, 3, 24)
      table.peer.setPreferredScrollableViewportSize(116 -> 100)

      val scroll    = new ScrollPane(table)
      scroll.border = null

      val ggAdd = Button("+")(addItemWithDialog())
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      val ggDelete: Button = Button("\u2212")(removeSelectedItem())
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      val butPanel  = new FlowPanel(ggAdd, ggDelete)

      comp          = new BorderPanel {
        add(scroll  , BorderPanel.Position.Center)
        add(butPanel, BorderPanel.Position.South )
      }
    }

    def dispose()(implicit tx: S#Tx) = ()

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
  }
}