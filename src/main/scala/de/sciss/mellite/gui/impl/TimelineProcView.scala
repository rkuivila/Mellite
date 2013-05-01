package de.sciss
package mellite
package gui
package impl

import synth.proc.{Proc, TimedProc, Sys}
import lucre.{stm, expr}
import span.{Span, SpanLike}
import expr.Expr
import synth.expr.SpanLikes
import language.implicitConversions

object TimelineProcView {
  def apply[S <: Sys[S]](timed: TimedProc[S])(implicit tx: S#Tx): TimelineProcView[S] = {
    val span  = timed.span
    val proc  = timed.value
    import SpanLikes._
    new Impl(tx.newHandle(span), tx.newHandle(proc), span.value, proc.name.value, None)
  }

  private final class Impl[S <: Sys[S]](val spanSource: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val procSource: stm.Source[S#Tx, Proc[S]],
                                        var span: SpanLike, var name: String, var sono: Option[sonogram.Overview])
    extends TimelineProcView[S]

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
  var name: String
  var sono: Option[sonogram.Overview]
}