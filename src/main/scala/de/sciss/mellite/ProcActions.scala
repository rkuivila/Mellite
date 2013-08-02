package de.sciss
package mellite

import lucre.expr.Expr
import span.{Span, SpanLike}
import de.sciss.synth.proc.{ProcKeys, Scan, Grapheme, Sys, Proc}
import synth.expr.ExprImplicits
import de.sciss.lucre.bitemp.BiExpr
import de.sciss.audiowidgets.TimelineModel

object ProcActions {
  private val MinDur    = 32

  final case class Resize(deltaStart: Long, deltaStop: Long)

  /** Queries the audio region's grapheme segment start and audio element. */
  def getAudioRegion[S <: Sys[S]](span: Expr[S, SpanLike], proc: Proc[S])
                                 (implicit tx: S#Tx): Option[(Expr[S, Long], Grapheme.Elem.Audio[S])] = {
    span.value match {
      case Span.HasStart(frame) =>
        for {
          scan <- proc.scans.get(ProcKeys.graphAudio)
          Scan.Link.Grapheme(g) <- scan.source
          BiExpr(time, audio: Grapheme.Elem.Audio[S]) <- g.at(frame)
        } yield (time, audio)

      case _ => None
    }
  }

  def resize[S <: Sys[S]](span: Expr[S, SpanLike], proc: Proc[S], amount: Resize, timelineModel: TimelineModel)
                         (implicit tx: S#Tx): Unit = {
    import amount._

    val oldSpan   = span.value
    val minStart  = timelineModel.bounds.start
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

      val (dStartCC, dStopCC) = getAudioRegion(span, proc) match {
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