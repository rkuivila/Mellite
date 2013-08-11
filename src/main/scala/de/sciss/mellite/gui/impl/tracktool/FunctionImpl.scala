/*
 *  FunctionImpl.scala
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
import de.sciss.span.{SpanLike, Span}
import de.sciss.lucre.expr.Expr

final class FunctionImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicRegion[S, TrackTool.Function] {

  import TrackTool._

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)
  val name          = "Function"
  val icon          = ToolsImpl.getIcon("hresize")

  protected def dialog(): Option[Function] = None // not yet supported

  protected def dragToParam(d: Drag): Function = {
    val (usesStart, usesStop) = d.firstRegion.span match {
      case Span.From (_)      => (true, false)
      case Span.Until(_)      => (false, true)
      case Span(start, stop)  =>
        val s = math.abs(d.firstPos - start) < math.abs(d.firstPos - stop)
        (s, !s)
      case _                  => (false, false)
    }
    val (dStart, dStop) = if (usesStart) {
      (d.currentPos - d.firstPos, 0L)
    } else if (usesStop) {
      (0L, d.currentPos - d.firstPos)
    } else {
      (0L, 0L)
    }
    ??? // Function(dStart, dStop)
  }

  protected def commitProc(drag: Function)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit =
    ??? // ProcActions.resize(span, proc, drag, canvas.timelineModel)
}
