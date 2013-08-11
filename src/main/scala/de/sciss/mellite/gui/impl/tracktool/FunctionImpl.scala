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
import de.sciss.synth.proc.{Attribute, ProcKeys, Proc, Sys}
import de.sciss.span.Span
import java.awt.event.MouseEvent
import de.sciss.synth.expr.{Ints, Spans}

final class FunctionImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends RegionLike[S, TrackTool.Function] with Dragging[S, TrackTool.Function] {

  import TrackTool._

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  val name          = "Function"
  val icon          = ToolsImpl.getIcon("function")

  protected type Initial = Unit

  protected def handlePress(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[timeline.ProcView[S]]): Unit = {
    handleMouseSelection(e, regionOpt)
    regionOpt match {
      case Some(region) =>
        if (e.getClickCount == 2) {
          println("Edit TODO")
        }

      case _  => new Drag(e, hitTrack, pos, ())
    }
  }

  protected def dragToParam(d: Drag): Function = {
    val dStart  = math.min(d.firstPos, d.currentPos)
    val dStop   = math.max(dStart + BasicRegion.MinDur, math.max(d.firstPos, d.currentPos))
    Function(d.firstTrack, Span(dStart, dStop))
  }

  def commit(drag: Function)(implicit tx: S#Tx): Unit =
    canvas.group.modifiableOption.foreach { g =>
      val span  = Spans.newVar(Spans.newConst(drag.span))
      val p     = Proc[S]
      p.attributes.put(ProcKeys.attrTrack, Attribute.Int(Ints.newVar(Ints.newConst(drag.track))))
      g.add(span, p)
      log(s"Added function region $p, span = ${drag.span}, track = ${drag.track}")

      // canvas.selectionModel.clear()
      // canvas.selectionModel += ?
    }
}
