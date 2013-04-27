package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.{Proc, TimedProc, Sys}
import de.sciss.lucre.stm
import de.sciss.span.{Span, SpanLike}
import de.sciss.lucre.expr.Expr
import de.sciss.synth.expr.SpanLikes
import language.implicitConversions

object TimelineProcView {
  def apply[S <: Sys[S]](timed: TimedProc[S])(implicit tx: S#Tx): TimelineProcView[S] = {
    val span  = timed.span
    val proc  = timed.value
    import SpanLikes._
    new Impl(tx.newHandle(span), tx.newHandle(proc), span.value, proc.name.value)
  }

  private final class Impl[S <: Sys[S]](val spanSource: stm.Source[S#Tx, Expr[S, SpanLike]],
                                        val procSource: stm.Source[S#Tx, Proc[S]],
                                        var span: SpanLike, var name: String)
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
}