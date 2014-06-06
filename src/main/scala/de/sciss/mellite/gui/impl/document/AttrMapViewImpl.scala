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

package de.sciss.mellite.gui
package impl
package document

import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm
import de.sciss.lucre.swing.impl.ComponentHolder
import scala.swing.{ScrollPane, Table}
import de.sciss.lucre.swing.deferTx
import de.sciss.desktop.UndoManager
import de.sciss.lucre.synth.Sys
import javax.swing.table.AbstractTableModel
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.annotation.switch

object AttrMapViewImpl {
  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      undoManager: UndoManager): AttrMapView[S] = {
    val map   = obj.attr
    val objH  = tx.newHandle(obj)

    val list0 = map.iterator.map {
      case (key, value) =>
        val view = ObjView(obj)
        (key, view)
    } .toIndexedSeq

    val res   = new Impl(objH, list0)

    deferTx {
      res.guiInit()
    }
    res
  }

  private final class Impl[S <: Sys[S]](mapH: stm.Source[S#Tx, Obj[S]],
                                        list0: Vec[(String, ObjView[S])])(implicit val cursor: stm.Cursor[S],
                                        val undoManager: UndoManager)
    extends AttrMapView[S] with ComponentHolder[ScrollPane] {

    def dispose()(implicit tx: S#Tx): Unit = {
      // ???
      println("NOT YET IMPLEMENTED: AttrMapViewImpl dispose")
    }

    private var model = list0

    private var tab: Table = _

    def guiInit(): Unit = {
      tab = new Table
      tab.model = new AbstractTableModel {
        def getRowCount   : Int = model.size
        def getColumnCount: Int = 2

        def getValueAt(row: Int, col: Int): AnyRef = (col: @switch) match {
          case 0 => model(row)._1
          case 1 => model(row)._2.name
        }
      }
      val scroll  = new ScrollPane(tab)
      component   = scroll
    }

    def selection: List[(String, ObjView[S])] =
      tab.selection.rows.toList.sorted.map(model.apply)
  }
}
