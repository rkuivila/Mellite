/*
 *  IntObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.lucre.expr.IntObj
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.synth.Sys
import de.sciss.swingplus.Spinner
import de.sciss.synth.proc.Confluent
import de.sciss.synth.proc.Implicits._

import scala.util.Try

object IntObjView extends ListObjView.Factory {
  type E[~ <: stm.Sys[~]] = IntObj[~]
  val icon      = ObjViewImpl.raphaelIcon(Shapes.IntegerNumber)
  val prefix    = "Int"
  def humanName = prefix
  def tpe = IntObj

  def category = ObjView.categPrimitives

  def mkListView[S <: Sys[S]](obj: IntObj[S])(implicit tx: S#Tx): IntObjView[S] with ListObjView[S] = {
    val ex          = obj
    val value       = ex.value
    val isEditable  = ex match {
      case IntObj.Var(_)  => true
      case _            => false
    }
    val isViewable  = tx.isInstanceOf[Confluent.Txn]
    new Impl(tx.newHandle(obj), value, isEditable = isEditable, isViewable = isViewable).initAttrs(obj)
  }

  type Config[S <: stm.Sys[S]] = ObjViewImpl.PrimitiveConfig[Int]

  def hasMakeDialog = true

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val model     = new SpinnerNumberModel(0, Int.MinValue, Int.MaxValue, 1)
    val ggValue   = new Spinner(model)
    ObjViewImpl.primitiveConfig[S, Int](window, tpe = prefix, ggValue = ggValue,
      prepare = Some(model.getNumber.intValue()))
  }

  def makeObj[S <: Sys[S]](config: (String, Int))(implicit tx: S#Tx): List[Obj[S]] = {
    val (name, value) = config
    val obj = IntObj.newVar(IntObj.newConst[S](value))
    obj.name = name
    obj :: Nil
  }

  final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, IntObj[S]],
                                var value: Int,
                                override val isEditable: Boolean, val isViewable: Boolean)
    extends IntObjView[S]
    with ListObjView /* .Int */[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.SimpleExpr[S, Int, IntObj]
    with ListObjViewImpl.StringRenderer
    /* with NonViewable[S] */ {

    override def obj(implicit tx: S#Tx) = objH()

    type E[~ <: stm.Sys[~]] = IntObj[~]

    def factory = IntObjView

    val exprType = IntObj

    def expr(implicit tx: S#Tx) = obj

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
trait IntObjView[S <: stm.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): IntObj[S]
}