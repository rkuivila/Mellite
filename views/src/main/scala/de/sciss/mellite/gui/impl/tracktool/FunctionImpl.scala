/*
 *  FunctionImpl.scala
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
import de.sciss.synth.proc.{Obj, Proc, IntElem}
import de.sciss.span.Span
import java.awt.event.MouseEvent
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.{Int => IntEx}
import de.sciss.lucre.bitemp.{Span => SpanEx}

final class FunctionImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends RegionLike[S, TrackTool.Function] with Dragging[S, TrackTool.Function] {

  import TrackTool.{Cursor => _, _}

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  val name          = "Function"
  val icon          = ToolsImpl.getIcon("function")

  protected type Initial = Unit

  protected def handlePress(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineObjView[S]]): Unit = {
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
    canvas.timeline.modifiableOption.foreach { g =>
      val span  = SpanEx.newVar[S](SpanEx.newConst(drag.span))
      val p     = Proc[S]
      val obj   = Obj(Proc.Elem(p))
      obj.attr.put(TimelineObjView.attrTrackIndex, Obj(IntElem(IntEx.newVar(IntEx.newConst(drag.track)))))
      g.add(span, obj)
      log(s"Added function region $p, span = ${drag.span}, track = ${drag.track}")

      // canvas.selectionModel.clear()
      // canvas.selectionModel += ?
    }
}
