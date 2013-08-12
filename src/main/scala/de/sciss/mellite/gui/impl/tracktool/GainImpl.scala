/*
 *  GainImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
import de.sciss.synth.proc.{Proc, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.span.SpanLike
import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth

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

  protected def commitProc(drag: Gain)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit = {
    import drag._
    val imp = ExprImplicits[S]
    import imp._
    // println(s"gain : commitProc. factor = $factor")
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
