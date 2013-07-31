package de.sciss
package mellite
package gui
package impl

import de.sciss.synth.proc.{ProcKeys, Attribute, Proc, Sys}
import de.sciss.model.impl.ModelImpl
import java.awt.{Point, Toolkit}
import java.awt.event.MouseEvent
import de.sciss.lucre.expr.Expr
import de.sciss.span.SpanLike
import de.sciss.synth.expr.Booleans
import TrackTool.Mute

object TrackMuteToolImpl {
  private lazy val cursor = {
    val tk = Toolkit.getDefaultToolkit
    val img = tk.createImage(Mellite.getClass.getResource("cursor-mute.png"))
    tk.createCustomCursor(img, new Point(4, 4), "Mute")
  }
}
final class TrackMuteToolImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends TrackRegionToolImpl[S, Mute] with ModelImpl[TrackTool.Update[Mute]] {

  def defaultCursor = TrackMuteToolImpl.cursor
  val name          = "Mute"
  val icon          = TrackToolsImpl.getIcon("mute")

  protected def commitProc(mute: Mute)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx) {
    val attr      = proc.attributes
    attr[Attribute.Boolean](ProcKeys.attrMute) match {
      // XXX TODO: Booleans should have `not` operator
      case Some(Expr.Var(vr)) => vr.transform { old => val vOld = old.value; Booleans.newConst(!vOld) }
      case _                  => attr.put(ProcKeys.attrMute, Attribute.Boolean(Booleans.newVar(Booleans.newConst(true))))
    }
  }

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView[S]) {
    dispatch(TrackTool.Adjust(Mute(!region.muted)))
  }
}
