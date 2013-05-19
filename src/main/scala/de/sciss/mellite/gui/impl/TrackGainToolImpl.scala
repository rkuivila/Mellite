package de.sciss
package mellite
package gui
package impl

import java.awt.Cursor
import de.sciss.synth.proc.{Proc, Sys}
import de.sciss.lucre.expr.Expr
import de.sciss.span.SpanLike
import de.sciss.synth.expr.ExprImplicits

final class TrackGainToolImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends BasicTrackRegionTool[S, TrackTool.Gain] {

  import TrackTool._

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.N_RESIZE_CURSOR)
  val name          = "Gain"
  val icon          = TrackToolsImpl.getIcon("vresize")

  protected def dialog(): Option[Gain] = None // not yet supported

  override protected def dragStarted(d: Drag): Boolean =
    d.currentEvent.getY != d.firstEvent.getY

  protected def dragToParam(d: Drag): Gain = {
    val dy = d.firstEvent.getY - d.currentEvent.getY
    // use 0.1 dB per pixel. eventually we could use modifier keys...
    import synth._
    val factor = (dy.toFloat / 10).dbamp
    Gain(factor)
  }

  protected def commitProc(drag: Gain)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx) {
    import drag._
    val imp = ExprImplicits[S]
    import imp._
    // println(s"gain : commitProc. factor = $factor")
    ProcActions.getAudioRegion(span, proc) match {
      case Some((gtime, audio)) => // audio region
        // println(s"audio.gain = ${audio.gain}")
        audio.gain match {
          case Expr.Var(vr) =>
            // println(s"old value ${vr.value}")
            vr.transform(_ * factor)
            // println(s"new value ${vr.value}")
          case _ =>
        }
      case _ =>
    }
  }
}
