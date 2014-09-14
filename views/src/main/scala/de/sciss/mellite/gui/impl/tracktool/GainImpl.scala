/*
 *  GainImpl.scala
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
package impl
package tracktool

import java.awt.Cursor
import javax.swing.undo.UndoableEdit
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.mellite.gui.edit.EditAttrMap
import de.sciss.synth.proc.{DoubleElem, ObjKeys, Obj, ExprImplicits}
import de.sciss.lucre.expr.{Double => DoubleEx}
import de.sciss.span.SpanLike
import de.sciss.synth
import de.sciss.lucre.synth.Sys

final class GainImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicRegion[S, TrackTool.Gain] {

  import TrackTool.{Cursor => _, _}

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)
  val name          = "Gain"
  val icon          = ToolsImpl.getIcon("vresize")

  protected def dialog(): Option[Gain] = None // not yet supported

  override protected def dragStarted(d: Drag): Boolean =
    d.currentEvent.getY != d.firstEvent.getY

  protected def dragToParam(d: Drag): Gain = {
    val dy = d.firstEvent.getY - d.currentEvent.getY
    // use 0.1 dB per pixel. eventually we could use modifier keys...
    import synth._
    val factor = (dy.toFloat / 10).dbamp
    Gain(factor)
  }

  protected def commitObj(drag: Gain)(span: Expr[S, SpanLike], obj: Obj[S])
                         (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    import drag.factor
    // ProcActions.adjustGain(obj, drag.factor)
    val imp = ExprImplicits[S]
    import imp._
    if (factor == 1f) None else {
      val newGain: Expr[S, Double] = obj.attr.expr[Double](ObjKeys.attrGain) match {
        case Some(Expr.Var(vr)) => vr() * factor.toDouble
        case other =>
          other.fold(1.0)(_.value) * factor
      }
      val newGainOpt = if (newGain == DoubleEx.newConst[S](1.0)) None else Some(newGain)
      import DoubleEx.serializer
      val edit = EditAttrMap.expr(s"Adjust $name", obj, ObjKeys.attrGain, newGainOpt) { ex =>
        DoubleElem(DoubleEx.newVar(ex))
      }
      Some(edit)
    }
  }
}
