/*
 *  EditAttrMap.scala
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
package edit

import de.sciss.lucre.{stm, event => evt}
import evt.Sys
import javax.swing.undo.{UndoableEdit, AbstractUndoableEdit}
import de.sciss.synth.proc.Obj

object EditAttrMap {
  def apply[S <: Sys[S]](name: String, obj: Obj[S], key: String, value: Option[Obj[S]])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val before    = obj.attr.get(key)
    val objH      = tx.newHandle(obj)
    val beforeH   = tx.newHandle(before)
    val nowH      = tx.newHandle(value)
    val res       = new Impl(name, key, objH, beforeH, nowH)
    res.perform()
    res
  }

  private[edit] final class Impl[S <: Sys[S]](name: String, key: String,
                                              objH   : stm.Source[S#Tx, Obj[S]],
                                              beforeH: stm.Source[S#Tx, Option[Obj[S]]],
                                              nowH   : stm.Source[S#Tx, Option[Obj[S]]])(implicit cursor: stm.Cursor[S])
    extends AbstractUndoableEdit {

    override def undo(): Unit = {
      super.undo()
      cursor.step { implicit tx => perform(beforeH) }
    }

    override def redo(): Unit = {
      super.redo()
      cursor.step { implicit tx => perform() }
    }

    private def perform(valueH: stm.Source[S#Tx, Option[Obj[S]]])(implicit tx: S#Tx): Unit =
      cursor.step { implicit tx =>
        val map = objH().attr
        valueH().fold[Unit] {
          map.remove(key)
        } { obj =>
          map.put(key, obj)
        }
      }

    def perform()(implicit tx: S#Tx): Unit = perform(nowH)

    override def getPresentationName = name
  }
}
