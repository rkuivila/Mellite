package de.sciss.mellite
package gui

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.span.SpanLike
import de.sciss.synth.proc.{FadeSpec, Obj, Elem}

import scala.language.higherKinds

object TimelineObjView {
  trait Factory {
    //    def prefix: String
    //    def icon  : Icon
    def typeID: Int

    type E[~ <: Sys[~]] <: Elem[~]

    def apply[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): TimelineObjView[S]
  }

  def addFactory(f: Factory): Unit = ??? // Impl.addFactory(f)

  def factories: Iterable[Factory] = ??? // Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = ??? // Impl(obj)

  // ---- specialization ----

  final val attrTrackIndex  = "track-index"
  final val attrTrackHeight = "track-height"

  trait HasMute {
    var muted: Boolean
  }

  trait HasGain {
    var gain: Double
  }

  trait HasFade {
    var fadeIn : FadeSpec
    var fadeOut: FadeSpec
  }
}
trait TimelineObjView[S <: Sys[S]] {
  /** The proc's name or a place holder name if no name is set. */
  //def name: String

  var nameOption: Option[String]

  def span: stm.Source[S#Tx, Expr[S, SpanLike]]
  def obj : stm.Source[S#Tx, Obj [S]]

  var spanValue: SpanLike

  var trackIndex : Int
  var trackHeight: Int
}