/*
 *  BooleanCheckBoxView.scala
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

package de.sciss.mellite.gui

import de.sciss.desktop.UndoManager
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.swing.{StringFieldView, View}
import de.sciss.lucre.{expr, stm}
import de.sciss.model.Change
import de.sciss.serial.Serializer
import impl.component.{BooleanCheckBoxViewImpl => Impl}

import scala.swing.CheckBox

// XXX TODO - move to LucreSwing
object BooleanCheckBoxView {
  def apply[S <: Sys[S]](expr: Expr[S, Boolean], name: String)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], undoManager: UndoManager): BooleanCheckBoxView[S] =
    Impl.fromExpr(expr, name = name)

  def fromMap[S <: Sys[S], A](map: expr.Map[S, A, Expr[S, Boolean], Change[Boolean]], key: A, default: Boolean,
                              name: String)
                             (implicit tx: S#Tx, keySerializer: Serializer[S#Tx, S#Acc, A],
                              cursor: stm.Cursor[S], undoManager: UndoManager): BooleanCheckBoxView[S] =
    Impl.fromMap(map, key = key, default = default, name = name)
}
trait BooleanCheckBoxView[S <: Sys[S]] extends View[S] {
  override def component: CheckBox
}