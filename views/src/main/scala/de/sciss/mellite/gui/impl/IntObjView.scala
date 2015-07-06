/*
 *  IntObjView.scala
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
package impl

import javax.swing.SpinnerNumberModel

import de.sciss.desktop
import de.sciss.lucre.expr.{Expr, Int => IntEx}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.swingplus.Spinner
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.ElemImpl
import de.sciss.synth.proc.{Confluent, IntElem, Obj}

import scala.util.Try

object IntObjView extends ListObjView.Factory {
  type E[S <: evt.Sys[S]] = IntElem[S]
  val icon      = ObjViewImpl.raphaelIcon(Shapes.IntegerNumbers)
  val prefix    = "Int"
  def humanName = prefix
  def typeID    = ElemImpl.Int.typeID

  def category = ObjView.categPrimitives

  def mkListView[S <: Sys[S]](obj: Obj.T[S, IntElem])(implicit tx: S#Tx): IntObjView[S] with ListObjView[S] = {
    val ex          = obj.elem.peer
    val value       = ex.value
    val isEditable  = ex match {
      case Expr.Var(_)  => true
      case _            => false
    }
    val isViewable  = tx.isInstanceOf[Confluent.Txn]
    new Impl(tx.newHandle(obj), value, isEditable = isEditable, isViewable = isViewable).initAttrs(obj)
  }

  type Config[S <: evt.Sys[S]] = ObjViewImpl.PrimitiveConfig[Int]

  def hasMakeDialog = true

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val model     = new SpinnerNumberModel(0, Int.MinValue, Int.MaxValue, 1)
    val ggValue   = new Spinner(model)
    ObjViewImpl.primitiveConfig[S, Int](window, tpe = prefix, ggValue = ggValue,
      prepare = Some(model.getNumber.intValue()))
  }

  def makeObj[S <: Sys[S]](config: (String, Int))(implicit tx: S#Tx): List[Obj[S]] = {
    import proc.Implicits._
    val (name, value) = config
    val obj = Obj(IntElem(IntEx.newVar(IntEx.newConst[S](value))))
    obj.name = name
    obj :: Nil
  }

  final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, IntElem]],
                                var value: Int,
                                override val isEditable: Boolean, val isViewable: Boolean)
    extends IntObjView[S]
    with ListObjView /* .Int */[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.SimpleExpr[S, Int]
    with ListObjViewImpl.StringRenderer
    /* with NonViewable[S] */ {

    type E[~ <: evt.Sys[~]] = IntElem[~]

    def factory = IntObjView

    def exprType = IntEx

    def expr(implicit tx: S#Tx) = obj().elem.peer

    def convertEditValue(v: Any): Option[Int] = v match {
      case num: Int  => Some(num)
      case s: String => Try(s.toInt).toOption
    }

    def testValue(v: Any): Option[Int] = v match {
      case i: Int  => Some(i)
      case _        => None
    }
  }
}
trait IntObjView[S <: evt.Sys[S]] extends ObjView[S] {
  override def obj: stm.Source[S#Tx, IntElem.Obj[S]]
}