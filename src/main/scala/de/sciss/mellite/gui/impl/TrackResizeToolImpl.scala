package de.sciss
package mellite
package gui
package impl

import java.awt.Cursor
import de.sciss.synth.proc.{Proc, Sys}
import de.sciss.span.{SpanLike, Span}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.lucre.expr.Expr

final class TrackResizeToolImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicTrackRegionTool[S, TrackTool.Resize] {

  import TrackTool._
  import BasicTrackRegionTool.MinDur

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.W_RESIZE_CURSOR)
  val name          = "Resize"
  val icon          = TrackToolsImpl.getIcon("hresize")

  protected def dialog(): Option[Resize] = None // not yet supported

  protected def dragToParam(d: Drag): Resize = {
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
    Resize(dStart, dStop)
  }

  protected def commitProc(drag: Resize)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx) {
    import drag._

    val oldSpan   = span.value
    val minStart  = canvas.timelineModel.bounds.start
    val dStartC   = if (deltaStart >= 0) deltaStart else oldSpan match {
      case Span.HasStart(oldStart)  => math.max(-(oldStart - minStart)         , deltaStart)
      case _ => 0L
    }
    val dStopC   = if (deltaStop >= 0) deltaStop else oldSpan match {
      case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + MinDur), deltaStop)
      case _ => 0L
    }

    if (dStartC != 0L || dStopC != 0L) {
      val imp = ExprImplicits[S]
      import imp._

      val (dStartCC, dStopCC) = TimelineProcView.getAudioRegion(span, proc) match {
        //        case Some((gtime, audio)) => // audio region
        //          val gtimeV  = gtime.value
        //          val audioV  = audio.value
        //          dStartC

        case _ => // other proc
          (dStartC, dStopC)
      }

      span match {
        case Expr.Var(s) =>
          s.transform { oldSpan =>
            oldSpan.value match {
              case Span.From(start)   => Span.From(start + dStartCC)
              case Span.Until(stop)   => Span.Until(stop  + dStopCC)
              case Span(start, stop)  =>
                val newStart = start + dStartCC
                Span(newStart, math.max(newStart + MinDur, stop + dStopCC))
              case other => other
            }
          }
      }
    }
  }
}
