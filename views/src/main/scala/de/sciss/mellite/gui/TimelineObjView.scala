/*
 *  TimelineObjView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.IdentifierMap
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.impl.ProcObjView
import de.sciss.mellite.gui.impl.timeline.{TimelineObjViewImpl => Impl}
import de.sciss.span.{Span, SpanLike}
import de.sciss.synth.proc.{FadeSpec, Obj, Timeline}

import scala.language.{higherKinds, implicitConversions}

object TimelineObjView {
  type SelectionModel[S <: evt.Sys[S]] = gui.SelectionModel[S, TimelineObjView[S]]

  final val Unnamed = "<unnamed>"

  implicit def span[S <: evt.Sys[S]](view: TimelineObjView[S]): (Long, Long) = {
    view.spanValue match {
      case Span(start, stop)  => (start, stop)
      case Span.From(start)   => (start, Long.MaxValue)
      case Span.Until(stop)   => (Long.MinValue, stop)
      case Span.All           => (Long.MinValue, Long.MaxValue)
      case Span.Void          => (Long.MinValue, Long.MinValue)
    }
  }

  type Map[S <: evt.Sys[S]] = IdentifierMap[S#ID, S#Tx, TimelineObjView[S]]

  trait Context[S <: evt.Sys[S]] {
    /** A map from `TimedProc` ids to their views. This is used to establish scan links. */
    def viewMap: Map[S]
    /** A map from `Scan` ids to their keys and a handle on the timed-proc's id. */
    def scanMap: ProcObjView.ScanMap[S]
  }

  trait Factory extends ObjView.Factory {
    /** Creates a new timeline view
      *
      * @param id       the `TimedElem`'s identifier
      * @param span     the span on the timeline
      * @param obj      the object placed on the timeline
      */
    def mkTimelineView[S <: Sys[S]](id: S#ID, span: Expr[S, SpanLike], obj: Obj.T[S, E],
                                    context: TimelineObjView.Context[S])(implicit tx: S#Tx): TimelineObjView[S]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](timed: Timeline.Timed[S], context: Context[S])(implicit tx: S#Tx): TimelineObjView[S] =
    Impl(timed, context)

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
trait TimelineObjView[S <: evt.Sys[S]] extends ObjView[S] {
  // def span: stm.Source[S#Tx, Expr[S, SpanLike]]

  def spanH: stm.Source[S#Tx, Expr[S, SpanLike]]

  def span(implicit tx: S#Tx): Expr[S, SpanLike]

  def id(implicit tx: S#Tx): S#ID // Timeline.Timed[S]

  var spanValue: SpanLike

  var trackIndex : Int
  var trackHeight: Int
}