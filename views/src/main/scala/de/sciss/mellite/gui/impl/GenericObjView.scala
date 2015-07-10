package de.sciss.mellite
package gui
package impl

import de.sciss.desktop
import de.sciss.icons.raphael
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Cursor, Source}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.serial.Serializer
import de.sciss.span.SpanLike
import de.sciss.synth.proc
import de.sciss.synth.proc.Obj

import scala.swing.{Component, Label}

object GenericObjView extends ObjView.Factory {
  val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.No)
  val prefix    = "Generic"
  def humanName = prefix
  val category  = "None"
  val typeID    = 0

  type E[~ <: evt.Sys[~]]       = proc.Elem[~]
  type Config[S <: evt.Sys[S]]  = Unit

  def hasMakeDialog: Boolean = false

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: Cursor[S]): Option[Config[S]] = None

  def makeObj[S <: Sys[S]](config: Unit)(implicit tx: S#Tx): List[Obj[S]] = Nil

  def mkTimelineView[S <: Sys[S]](id: S#ID, span: Expr[S, SpanLike], obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = {
    implicit val spanLikeSer = SpanLikeEx.serializer[S]
    implicit val idSer: Serializer[S#Tx, S#Acc, S#ID] = ???
    val res = new TimelineImpl(tx.newHandle(id), tx.newHandle(span), tx.newHandle(obj)).initAttrs(span, obj)
    res
  }

  def mkListView[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ListObjView[S] =
    new ListImpl(tx.newHandle(obj)).initAttrs(obj)

  private trait Impl[S <: evt.Sys[S]] extends ObjViewImpl.Impl[S] {
    def factory = GenericObjView

    final def value: Any = ()

    final def configureRenderer(label: Label): Component = label

    final def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false
  }

  private final class ListImpl[S <: Sys[S]](protected val objH: stm.Source[S#Tx, Obj[S]])
    extends Impl[S] with ListObjView[S] with ListObjViewImpl.NonEditable[S] with ObjViewImpl.NonViewable[S]

  private final class TimelineImpl[S <: Sys[S]](protected val idH  : stm.Source[S#Tx, S#ID],
                                                protected val spanH: stm.Source[S#Tx, Expr[S, SpanLike]],
                                                protected val objH : stm.Source[S#Tx, Obj[S]])
    extends Impl[S] with TimelineObjViewImpl.BasicImpl[S] {

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: Cursor[S]): Option[Window[S]] = None

    def isViewable: Boolean = false
  }
}
