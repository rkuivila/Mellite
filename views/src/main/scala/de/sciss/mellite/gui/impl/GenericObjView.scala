package de.sciss.mellite
package gui
package impl

import de.sciss.desktop
import de.sciss.icons.raphael
import de.sciss.lucre.expr.SpanLikeObj
import de.sciss.lucre.stm.{Cursor, Obj}
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl

import scala.swing.{Component, Label}

object GenericObjView extends ObjView.Factory {
  val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.No)
  val prefix    = "Generic"
  def humanName = prefix
  val category  = "None"
  // val typeID    = 0
  def tpe: Obj.Type = ??? // RRR

  type E[~ <: stm.Sys[~]]       = Obj[~]
  type Config[S <: stm.Sys[S]]  = Unit

  def hasMakeDialog: Boolean = false

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: Cursor[S]): Option[Config[S]] = None

  def makeObj[S <: Sys[S]](config: Unit)(implicit tx: S#Tx): List[Obj[S]] = Nil

  def mkTimelineView[S <: Sys[S]](id: S#ID, span: SpanLikeObj[S], obj: Obj[S])(implicit tx: S#Tx): TimelineObjView[S] = {
    val res = new TimelineImpl(tx.newHandle(obj)).initAttrs(id, span, obj)
    res
  }

  def mkListView[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ListObjView[S] =
    new ListImpl(tx.newHandle(obj)).initAttrs(obj)

  private trait Impl[S <: stm.Sys[S]] extends ObjViewImpl.Impl[S] {
    def factory = GenericObjView

    final def value: Any = ()

    final def configureRenderer(label: Label): Component = label

    final def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = false
  }

  private final class ListImpl[S <: Sys[S]](val objH: stm.Source[S#Tx, Obj[S]])
    extends Impl[S] with ListObjView[S] with ListObjViewImpl.NonEditable[S] with ObjViewImpl.NonViewable[S]

  private final class TimelineImpl[S <: Sys[S]](val objH : stm.Source[S#Tx, Obj[S]])
    extends Impl[S] with TimelineObjViewImpl.BasicImpl[S] {

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: Cursor[S]): Option[Window[S]] = None

    def isViewable: Boolean = false
  }
}
