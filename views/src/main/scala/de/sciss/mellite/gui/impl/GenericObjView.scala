package de.sciss.mellite
package gui
package impl

import javax.swing.Icon

import de.sciss.icons.raphael
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Cursor, Source}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.span.SpanLike
import de.sciss.synth.proc.Obj

import scala.swing.{Component, Label}

object GenericObjView {
  val icon = ObjViewImpl.raphaelIcon(raphael.Shapes.No)

  def mkTimelineView[S <: Sys[S]](span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = {
    implicit val spanLikeSer = SpanLikeEx.serializer[S]
    val res = new TimelineImpl(tx.newHandle(span), tx.newHandle(obj), ObjViewImpl.nameOption(obj))
    TimelineObjViewImpl.initAttrs(span, obj, res)
    res
  }

  def mkListView[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ListObjView[S] =
    new ListImpl(tx.newHandle(obj), ObjViewImpl.nameOption(obj))

  private trait Impl[S <: evt.Sys[S]] extends ObjView[S] {
    final def prefix: String  = "Generic"
    final def typeID: Int     = 0
    final def icon  : Icon    = GenericObjView.icon

    final def value: Any = ()

    final def configureRenderer(label: Label): Component = label

    final def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false

    final def dispose()(implicit tx: S#Tx): Unit = ()
  }

  private final class ListImpl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj[S]], var nameOption: Option[String])
    extends Impl[S] with ListObjView[S] with ListObjViewImpl.NonEditable[S] with ObjViewImpl.NonViewable[S]

  private final class TimelineImpl[S <: Sys[S]](val span: Source[S#Tx, Expr[S, SpanLike]],
                                        val obj: stm.Source[S#Tx, Obj[S]],
                                        var nameOption: Option[String])
    extends Impl[S] with TimelineObjViewImpl.BasicImpl[S] {

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: Cursor[S]): Option[Window[S]] = None

    def isViewable: Boolean = false
  }
}
