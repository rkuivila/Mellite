/*
 *  MoveImpl.scala
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

import de.sciss.synth.proc.{ProcKeys, Proc, Attribute, Sys}
import java.awt.Cursor
import de.sciss.span.{SpanLike, Span}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.lucre.expr.Expr

final class MoveImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicRegion[S, TrackTool.Move] {

  import TrackTool._
  import BasicRegion.MinDur

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  val name          = "Move"
  val icon          = ToolsImpl.getIcon("openhand")

  protected def dragToParam(d: Drag): Move = {
    val eNow  = d.currentEvent
    val dtim0 = d.currentPos - d.firstPos
    val dtrk0 = d.currentTrack - d.firstTrack
    val (dtim, dtrk) = if (eNow.isShiftDown) { // constrain movement to either horizontal or vertical
      val eBefore = d.firstEvent
      if (math.abs(eNow.getX - eBefore.getX) > math.abs(eNow.getY - eBefore.getY)) {  // horizontal
        (dtim0, 0)
      } else {  // vertical
        (0L, dtrk0)
      }
    } else {  // unconstrained
      (dtim0, dtrk0)
    }

    Move(deltaTime = dtim, deltaTrack = dtrk, copy = d.currentEvent.isAltDown)
  }

  protected def commitProc(drag: Move)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit = {
    import drag._
    if (deltaTrack != 0) {
      // XXX TODO: could check for Expr.Const here and Expr.Var.
      // in the case of const, just overwrite, in the case of
      // var, check the value stored in the var, and update the var
      // instead (recursion). otherwise, it will be some combinatory
      // expression, and we could decide to construct a binary op instead!
      val attr = proc.attributes
      val expr = ExprImplicits[S]
      import expr._
      // attr[Attribute.Int[S]](ProcKeys.track).foreach {
      //   case Expr.Var(vr) => vr.transform(_ + deltaTrack)
      //   case _ =>
      // }

      // lucre.event.showLog = true
      // lucre.bitemp.impl.BiGroupImpl.showLog = true

      for (Expr.Var(t) <- attr[Attribute.Int](ProcKeys.attrTrack)) t.transform(_ + deltaTrack)

      // lucre.event.showLog = false
      // lucre.bitemp.impl.BiGroupImpl.showLog = false

      // val trackNew  = math.max(0, trackOld + deltaTrack)
      // attr.put(ProcKeys.track, Attribute.Int(Ints.newConst(trackNew)))
    }

    val oldSpan   = span.value
    val minStart  = canvas.timelineModel.bounds.start
    val deltaC    = if (deltaTime >= 0) deltaTime else oldSpan match {
      case Span.HasStart(oldStart)  => math.max(-(oldStart - minStart)         , deltaTime)
      case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + MinDur), deltaTime)
    }
    if (deltaC != 0L) {
      val imp = ExprImplicits[S]
      import imp._

      ProcActions.getAudioRegion(span, proc) match {
        case Some((gtime, audio)) => // audio region
          (span, gtime) match {
            case (Expr.Var(t1), Expr.Var(t2)) =>
              t1.transform(_ shift deltaC)
              t2.transform(_ + deltaC) // XXX TODO: actually should shift the segment as well, i.e. the ceil time?

            case _ =>
          }
        case _ => // other proc
          span match {
            case Expr.Var(s) => s.transform(_ shift deltaC)
            case _ =>
          }
      }
    }
  }

  protected def dialog(): Option[Move] = {
    println("Not yet implemented - movement dialog")
    //    val box             = Box.createHorizontalBox
    //    val timeTrans       = new DefaultUnitTranslator()
    //    val ggTime          = new BasicParamField(timeTrans)
    //    val spcTimeHHMMSSD  = new ParamSpace(Double.NegativeInfinity, Double.PositiveInfinity, 0.0, 1, 3, 0.0,
    //      ParamSpace.TIME | ParamSpace.SECS | ParamSpace.HHMMSS | ParamSpace.OFF)
    //    ggTime.addSpace(spcTimeHHMMSSD)
    //    ggTime.addSpace(ParamSpace.spcTimeSmpsD)
    //    ggTime.addSpace(ParamSpace.spcTimeMillisD)
    //    GUI.setInitialDialogFocus(ggTime)
    //    box.add(new JLabel("Move by:"))
    //    box.add(Box.createHorizontalStrut(8))
    //    box.add(ggTime)
    //
    //    val tl = timelineModel.timeline
    //    timeTrans.setLengthAndRate(tl.span.length, tl.rate)
    //    if (showDialog(box)) {
    //      val delta = timeTrans.translate(ggTime.value, ParamSpace.spcTimeSmpsD).value.toLong
    //      Some(Move(delta, 0, copy = false))
    //    } else
    None
  }
}
