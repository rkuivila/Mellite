/*
 *  GainImpl.scala
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
package impl
package tracktool

import java.awt.Cursor
import de.sciss.synth.proc.{ExprImplicits, Proc}
import de.sciss.lucre.expr.Expr
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

  protected def commitProc(drag: Gain)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit =
    ProcActions.adjustGain(proc, drag.factor)

  private def commitOld(drag: Gain)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit = {
    import drag._
    val imp = ExprImplicits[S]
    import imp._
    ProcActions.getAudioRegion(span, proc) match {
      case Some((gtime, audio)) => // audio region
        // println(s"audio.gain = ${audio.gain}")
        audio.gain match {
          case Expr.Var(vr) =>
            // println(s"old value ${vr.value}")
            vr.transform(_ * factor)
            // println(s"new value ${vr.value}")
          case _ =>
        }
      case _ =>
    }
  }
}
