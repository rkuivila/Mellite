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
import de.sciss.synth.proc.Sys
import scala.swing.{Table, Component}
import collection.immutable.{IndexedSeq => Vec}
import javax.swing.table.AbstractTableModel
import scala.annotation.switch

object GlobalProcsViewImpl {
  def apply[S <: Sys[S]](document: Document[S], group: Element.ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): GlobalProcsView[S] = {

    val view = new Impl[S]
    guiFromTx(view.guiInit())
    view
  }

  private final class Impl[S <: Sys[S]] extends GlobalProcsView[S] with ComponentHolder[Component] {

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

    def guiInit(): Unit = {
      val table   = new Table()
      table.model = tm
      comp        = table
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