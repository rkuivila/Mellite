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

import de.sciss.lucre.event.Sys
import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm
import de.sciss.lucre.swing.impl.ComponentHolder
import scala.swing.Table
import de.sciss.lucre.swing.deferTx
import de.sciss.desktop.UndoManager

object AttrMapViewImpl {
  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      undoManager: UndoManager): AttrMapView[S] = {
    val map   = obj.attr
    val objH  = tx.newHandle(obj)
    val res   = new Impl(objH)

    map.iterator.foreach {
      case (key, value) =>
    }

    deferTx {
      res.guiInit()
    }
    res
  }

  private final class Impl[S <: Sys[S]](mapH: stm.Source[S#Tx, Obj[S]])(implicit val cursor: stm.Cursor[S],
                                                                        val undoManager: UndoManager)
    extends AttrMapView[S] with ComponentHolder[Table] {

    def dispose()(implicit tx: S#Tx): Unit = ???

    def guiInit(): Unit = {
      val tab = new Table
      component = tab
    }
  }
}
