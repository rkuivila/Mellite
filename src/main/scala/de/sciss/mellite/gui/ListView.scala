/*
 *  ListView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import swing.Component
import de.sciss.lucre.{stm, expr, event => evt}
import stm.{Cursor, Disposable}
import impl.component.{ListViewImpl => Impl}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.serial.Serializer
import evt.Sys

object ListView {
  def apply[S <: Sys[S], Elem <: evt.Publisher[S, U], U](list: expr.List[S, Elem, U])(show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, expr.List[S, Elem, U]])
  : ListView[S, Elem, U] = Impl(list)(show)

  def empty[S <: Sys[S], Elem, U](show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, expr.List[S, Elem, U]])
  : ListView[S, Elem, U] = Impl.empty(show)

  sealed trait Update
  final case class SelectionChanged(current: Vec[Int]) extends Update
}
trait ListView[S <: Sys[S], Elem, U] extends Disposable[S#Tx] {
  def component: Component

  def guiReact(pf: PartialFunction[ListView.Update, Unit]): Removable

  def guiSelection: Vec[Int]

  def list(implicit tx: S#Tx): Option[expr.List[S, Elem, U]]
  def list_=(list: Option[expr.List[S, Elem, U]])(implicit tx: S#Tx): Unit
}
