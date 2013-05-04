package de.sciss
package mellite
package gui
package impl

import synth.proc.{Attribute, Grapheme, Scan, Proc, TimedProc, Sys}
import lucre.{stm, expr}
import span.{Span, SpanLike}
import expr.Expr
import synth.expr.SpanLikes
import language.implicitConversions

object TimelineProcView {
  def apply[S <: Sys[S]](timed: TimedProc[S])(implicit tx: S#Tx): TimelineProcView[S] = {
    val span  = timed.span
    val proc  = timed.value
    val spanV = span.value
    import SpanLikes._
    // println("--- scan keys:")
    // proc.scans.keys.foreach(println)
    val audio = proc.scans.get(TimelineView.AudioGraphemeKey).flatMap { scanw =>
      // println("--- has scan")
      scanw.source.flatMap {
        case Scan.Link.Grapheme(g) =>
          // println("--- scan is linked")
          spanV match {
            case Span.HasStart(frame) =>
              // println("--- has start")
              g.segment(frame) match {
                case Some(segm @ Grapheme.Segment.Audio(gspan, _)) /* if (gspan.start == frame) */ => Some(segm)
                // case Some(Grapheme.Segment.Audio(gspan, _audio)) =>
                //   // println(s"--- has audio segment $gspan offset ${_audio.offset}}; proc $spanV")
                //   // if (gspan == spanV) ... -> no, because segment will give as a Span.From(_) !
                //   if (gspan.start == frame) Some(_audio) else None
                case _ => None
              }
            case _ => None
          }
        case _ => None
      }
    }

    val attr  = proc.attributes

    val track = attr.get(ProcKeys.track) match {
      case Some(i: Attribute.Int[S]) => i.peer.value
      case _ => 0
    }

    val name = attr.get(ProcKeys.name) match {
      case Some(str: Attribute.String[S]) => str.peer.value
      case _ =>
        audio.map(_.value.artifact.nameWithoutExtension).getOrElse("<unnamed>")
    }

    new Impl(spanSource = tx.newHandle(span), procSource = tx.newHandle(proc),
      span = spanV, track = track, name = name, audio = audio)
  }

  private final class Impl[S <: Sys[S]](val spanSource: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val procSource: stm.Source[S#Tx, Proc[S]],
                                        var span: SpanLike, var track: Int, var name: String,
                                        var audio: Option[Grapheme.Segment.Audio])
    extends TimelineProcView[S] {

    var sono = Option.empty[sonogram.Overview]
  }

  implicit def span[S <: Sys[S]](view: TimelineProcView[S]): (Long, Long) = {
    view.span match {
      case Span(start, stop)  => (start, stop)
      case Span.From(start)   => (start, Long.MaxValue)
      case Span.Until(stop)   => (Long.MinValue, stop)
      case Span.All           => (Long.MinValue, Long.MaxValue)
      case Span.Void          => (Long.MinValue, Long.MinValue)
    }
  }
}
sealed trait TimelineProcView[S <: Sys[S]] {
  def spanSource: stm.Source[S#Tx, Expr[S, SpanLike]]
  def procSource: stm.Source[S#Tx, Proc[S]]

  var span: SpanLike
  var track: Int
  var name: String
  // var audio: Option[Grapheme.Value.Audio]
  var audio: Option[Grapheme.Segment.Audio]

  // EDT access only
  var sono: Option[sonogram.Overview]

  // def updateSpan(span: Expr[S, SpanLike])(implicit tx: S#Tx): Unit
}