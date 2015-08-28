/*
 *  AttrCellView.scala
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

import de.sciss.lucre.expr.{StringObj, Expr}
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.swing.CellView
import de.sciss.mellite.gui.impl.{AttrCellViewImpl => Impl}
import de.sciss.synth.proc.ObjKeys

import scala.language.higherKinds

object AttrCellView {
  def apply[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A]}](obj: Obj[S], key: String)
                                     (implicit tx: S#Tx, tpe: ExprType[A],
                                      companion: Elem.Companion[E]): CellView.Var[S, Option[A]] { type Repr = Option[Expr[S, A]] } = {
    new Impl.ModImpl[S, A, E](tx.newHandle(obj), key)
  }

  def name[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): CellView[S#Tx, String] = {
    implicit val stringEx = StringObj
    apply[S, String, StringObj](obj, ObjKeys.attrName).map(_.getOrElse("<unnamed>"))
  }
}