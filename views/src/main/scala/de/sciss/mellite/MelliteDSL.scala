package de.sciss.mellite

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.synth.proc.Timeline

object MelliteDSL {
  implicit class ExprAsVar[A, S <: Sys[S]](val `this`: Expr[S, A]) extends AnyVal { me =>
    import me.{`this` => ex}
    def asVar: Expr.Var[S, A] = Expr.Var.unapply(ex).getOrElse(sys.error(s"Not a variable: $ex"))
  }

  //  implicit class SpanLikeAsSpan[A, S <: Sys[S]](private val ex: Expr[S, SpanLike]) extends AnyVal {
  //    def asBounded: Expr[S, SpanLike] =
  //  }

  implicit class SecFrames(val `this`: Double) extends AnyVal { me =>
    import me.{`this` => d}
    def secframes: Long = (d * Timeline.SampleRate + 0.5).toLong
  }
}
