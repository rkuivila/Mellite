package de.sciss
package mellite
package gui
package impl

import de.sciss.synth.proc.{ProcKeys, Proc, Attribute, Sys}
import java.awt.Cursor
import de.sciss.span.{SpanLike, Span}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.synth.proc
import de.sciss.lucre.expr.Expr

final class TrackFadeToolImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicTrackRegionTool[S, TrackTool.Fade] {

  import TrackTool._
  import BasicTrackRegionTool.MinDur

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.NW_RESIZE_CURSOR)
  val name          = "Fade"
  val icon          = TrackToolsImpl.getIcon("fade")

  private var curvature = false

  protected def dragToParam(d: Drag): Fade = {
    val firstSpan = d.firstRegion.span
    val leftHand = firstSpan match {
      case Span(start, stop)  => math.abs(d.firstPos - start) < math.abs(d.firstPos - stop)
      case Span.From(start)   => true
      case Span.Until(stop)   => false
      case _                  => true
    }
    val (deltaTime, deltaCurve) = if (curvature) {
      val dc = (d.firstEvent.getY - d.currentEvent.getY) * 0.1f
      (0L, if (leftHand) -dc else dc)
    } else {
      (if (leftHand) d.currentPos - d.firstPos else d.firstPos - d.currentPos, 0f)
    }
    if (leftHand) Fade(deltaTime, 0L, deltaCurve, 0f)
    else Fade(0L, deltaTime, 0f, deltaCurve)
  }

  override protected def dragStarted(d: this.Drag): Boolean = {
    val result = super.dragStarted(d)
    if (result) {
      curvature = math.abs(d.currentEvent.getX - d.firstEvent.getX) <
        math.abs(d.currentEvent.getY - d.firstEvent.getY)
    }
    result
  }

  protected def commitProc(drag: Fade)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx) {
    import drag._
    println("NOT YET IMPLEMENTED") // XXX TODO
  }

  protected def dialog() = None // XXX TODO
}
