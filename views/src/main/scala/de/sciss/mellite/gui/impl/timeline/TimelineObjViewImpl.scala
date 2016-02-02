/*
 *  TimelineObjViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.expr.{IntObj, BooleanObj, DoubleObj, SpanLikeObj}
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm
import de.sciss.mellite.gui.TimelineObjView.{Context, Factory}
import de.sciss.mellite.gui.impl.{ActionView, GenericObjView, ObjViewImpl, ProcObjView}
import de.sciss.span.SpanLike
import de.sciss.synth.proc.{FadeSpec, ObjKeys, Timeline}

object TimelineObjViewImpl {
  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.tpe.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](timed: Timeline.Timed[S], context: Context[S])
                        (implicit tx: S#Tx): TimelineObjView[S] = {
    val span  = timed.span
    val obj   = timed.value
    val tid   = obj.tpe.typeID
    // getOrElse(sys.error(s"No view for type $tid"))
    map.get(tid).fold(GenericObjView.mkTimelineView(timed.id, span, obj)) { f =>
      f.mkTimelineView(timed.id, span, obj.asInstanceOf[f.E[S]], context)
    }
  }

  private var map = Map[Int, Factory](
    ProcObjView .tpe.typeID -> ProcObjView,
    ActionView  .tpe.typeID -> ActionView
  )

  // -------- Generic --------

//  def initGainAttrs[S <: stm.Sys[S]](span: SpanLikeObj[S], obj: Obj[S], view: TimelineObjView.HasGain)
//                                (implicit tx: S#Tx): Unit = {
//    val attr    = obj.attr
//    view.gain   = attr.$[DoubleObj](ObjKeys.attrGain).fold(1.0)(_.value)
//  }

//  def initMuteAttrs[S <: stm.Sys[S]](span: SpanLikeObj[S], obj: Obj[S], view: TimelineObjView.HasMute)
//                                (implicit tx: S#Tx): Unit = {
//    val attr    = obj.attr
//    view.muted  = attr.$[BooleanObj](ObjKeys.attrMute).exists(_.value)
//  }

//  def initFadeAttrs[S <: stm.Sys[S]](span: SpanLikeObj[S], obj: Obj[S], view: TimelineObjView.HasFade)
//                                (implicit tx: S#Tx): Unit = {
//    val attr          = obj.attr
//    view.fadeIn  = attr.$[FadeSpec.Obj](ObjKeys.attrFadeIn ).fold(TrackTool.EmptyFade)(_.value)
//    view.fadeOut = attr.$[FadeSpec.Obj](ObjKeys.attrFadeOut).fold(TrackTool.EmptyFade)(_.value)
//  }

  trait BasicImpl[S <: stm.Sys[S]] extends TimelineObjView[S] with ObjViewImpl.Impl[S] {
    var trackIndex  : Int = _
    var trackHeight : Int = _
    // var nameOption  : Option[String] = _
    var spanValue   : SpanLike = _
    var spanH       : stm.Source[S#Tx, SpanLikeObj[S]] = _

    protected var idH  : stm.Source[S#Tx, S#ID] = _

    def span(implicit tx: S#Tx) = spanH()
    def id  (implicit tx: S#Tx) = idH()

    def initAttrs(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): this.type = {
      val attr      = obj.attr

      implicit val intTpe = IntObj
      val trackIdxView = AttrCellView[S, Int, IntObj](attr, TimelineObjView.attrTrackIndex)
      disposables ::= trackIdxView.react { implicit tx => opt => deferTx {
        trackIndex = opt.getOrElse(0)
        dispatch(ObjView.Repaint(this))
      }}
      trackIndex   = trackIdxView().getOrElse(0)

      val trackHView = AttrCellView[S, Int, IntObj](attr, TimelineObjView.attrTrackHeight)
      disposables ::= trackHView.react { implicit tx => opt => deferTx {
        trackHeight = opt.getOrElse(4)
        dispatch(ObjView.Repaint(this))
      }}
      trackHeight   = trackHView().getOrElse(4)

      spanH         = tx.newHandle(span)
      spanValue     = span.value
      idH           = tx.newHandle(id)
      initAttrs(obj)
    }
  }

  trait HasGainImpl[S <: stm.Sys[S]] extends BasicImpl[S] with TimelineObjView.HasGain {
    var gain: Double = _

    override def initAttrs(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): this.type = {
      super.initAttrs(id, span, obj)

      implicit val doubleTpe = DoubleObj
      val gainView = AttrCellView[S, Double, DoubleObj](obj.attr, ObjKeys.attrGain)
      disposables ::= gainView.react { implicit tx => opt => deferTx {
        gain = opt.getOrElse(1.0)
        dispatch(ObjView.Repaint(this))
      }}
      gain = gainView().getOrElse(1.0)
      this
    }
  }

  trait HasMuteImpl[S <: stm.Sys[S]] extends BasicImpl[S] with TimelineObjView.HasMute {
    var muted: Boolean = _

    override def initAttrs(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): this.type = {
      super.initAttrs(id, span, obj)

      implicit val booleanTpe = BooleanObj
      val muteView = AttrCellView[S, Boolean, BooleanObj](obj.attr, ObjKeys.attrMute)
      disposables ::= muteView.react { implicit tx => opt => deferTx {
        muted = opt.getOrElse(false)
        dispatch(ObjView.Repaint(this))
      }}
      muted = muteView().getOrElse(false)
      this
    }
  }

  trait HasFadeImpl[S <: stm.Sys[S]] extends BasicImpl[S] with TimelineObjView.HasFade {
    var fadeIn : FadeSpec = _
    var fadeOut: FadeSpec = _

    override def initAttrs(id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): this.type = {
      super.initAttrs(id, span, obj)

      implicit val fadeTpe = FadeSpec.Obj
      val fadeInView = AttrCellView[S, FadeSpec, FadeSpec.Obj](obj.attr, ObjKeys.attrFadeIn)
      disposables ::= fadeInView.react { implicit tx => opt => deferTx {
        fadeIn = opt.getOrElse(TrackTool.EmptyFade)
        dispatch(ObjView.Repaint(this))
      }}
      val fadeOutView = AttrCellView[S, FadeSpec, FadeSpec.Obj](obj.attr, ObjKeys.attrFadeOut)
      disposables ::= fadeOutView.react { implicit tx => opt => deferTx {
        fadeOut = opt.getOrElse(TrackTool.EmptyFade)
        dispatch(ObjView.Repaint(this))
      }}
      fadeIn  = fadeInView ().getOrElse(TrackTool.EmptyFade)
      fadeOut = fadeOutView().getOrElse(TrackTool.EmptyFade)
      this
    }
  }
}