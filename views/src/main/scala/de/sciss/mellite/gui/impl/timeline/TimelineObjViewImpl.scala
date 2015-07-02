/*
 *  TimelineObjViewImpl.scala
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
package impl.timeline

import javax.swing.Icon

import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Cursor, Source}
import de.sciss.lucre.swing.Window
import de.sciss.mellite.gui.TimelineObjView.{Context, Factory}
import de.sciss.mellite.gui.impl.{ProcObjView, ActionView}
import de.sciss.span.SpanLike
import de.sciss.synth.proc.{BooleanElem, DoubleElem, FadeSpec, IntElem, Obj, ObjKeys, Timeline}

object TimelineObjViewImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](timed: Timeline.Timed[S], context: Context[S])
                        (implicit tx: S#Tx): TimelineObjView[S] = {
    val span  = timed.span
    val obj   = timed.value
    val tid   = obj.elem.typeID
    // getOrElse(sys.error(s"No view for type $tid"))
    ??? // map.get(tid).fold(Generic(span, obj))(f => f(timed.id, span, obj.asInstanceOf[Obj.T[S, f.E]], context))
  }

  private var map = Map[Int, Factory](
    ProcObjView .typeID -> ProcObjView,
    ActionView  .typeID -> ActionView
  )

  // -------- Generic --------

  def initAttrs[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S], view: TimelineObjView[S])
                            (implicit tx: S#Tx): Unit = {
    val attr          = obj.attr
    // view.nameOption   = attr[StringElem](ObjKeys        .attrName       ).map    (_.value)
    view.trackIndex   = attr[IntElem   ](TimelineObjView.attrTrackIndex ).fold(0)(_.value)
    view.trackHeight  = attr[IntElem   ](TimelineObjView.attrTrackHeight).fold(4)(_.value)
    view.spanValue    = span.value
  }

  def initGainAttrs[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S], view: TimelineObjView.HasGain)
                                (implicit tx: S#Tx): Unit = {
    val attr    = obj.attr
    view.gain   = attr[DoubleElem](ObjKeys.attrGain).fold(1.0)(_.value)
  }

  def initMuteAttrs[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S], view: TimelineObjView.HasMute)
                                (implicit tx: S#Tx): Unit = {
    val attr    = obj.attr
    view.muted  = attr[BooleanElem](ObjKeys.attrMute).exists(_.value)
  }

  def initFadeAttrs[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S], view: TimelineObjView.HasFade)
                                (implicit tx: S#Tx): Unit = {
    val attr          = obj.attr
    view.fadeIn  = attr[FadeSpec.Elem](ObjKeys.attrFadeIn ).fold(TrackTool.EmptyFade)(_.value)
    view.fadeOut = attr[FadeSpec.Elem](ObjKeys.attrFadeOut).fold(TrackTool.EmptyFade)(_.value)
  }

  trait BasicImpl[S <: Sys[S]] extends TimelineObjView[S] {
    var trackIndex  : Int = _
    var trackHeight : Int = _
    var nameOption  : Option[String] = _
    var spanValue   : SpanLike = _
  }

  import SpanLikeEx.serializer

  object Generic {
    def apply[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = {
      val res = new Generic.Impl(tx.newHandle(span), tx.newHandle(obj))
      initAttrs(span, obj, res)
      res
    }
    
    private final class Impl[S <: Sys[S]](val span: Source[S#Tx, Expr[S, SpanLike]], val obj: stm.Source[S#Tx, Obj[S]])
      extends BasicImpl[S] {

      def typeID: Int = ???

      def prefix: String = "Generic"

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: Cursor[S]): Option[Window[S]] = None

      def icon: Icon = ???

      def isViewable: Boolean = false

      def dispose()(implicit tx: S#Tx) = ()
    }
  }
}