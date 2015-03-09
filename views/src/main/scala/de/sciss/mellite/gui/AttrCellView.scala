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

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{ExprType, Expr}
import de.sciss.lucre.swing.CellView
import de.sciss.synth.proc.{ObjKeys, StringElem, Elem, Obj}
import impl.{AttrCellViewImpl => Impl}

import scala.language.higherKinds

object AttrCellView {
  def apply[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A]}](obj: Obj[S], key: String)
                                     (implicit tx: S#Tx, tpe: ExprType[A],
                                      companion: Elem.Companion[E]): CellView.Var[S, Option[A]] { type Repr = Option[Expr[S, A]] } = {
    new Impl.ModImpl[S, A, E](tx.newHandle(obj), key)
  }

  def name[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): CellView[S#Tx, String] = {
    implicit val stringEx = de.sciss.lucre.expr.String
    apply[S, String, StringElem](obj, ObjKeys.attrName).map(_.getOrElse("<unnamed>"))
  }
}