/*
 *  ActionView.scala
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
package impl

import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.icons.raphael
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeEx}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Source
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.span.SpanLike
import de.sciss.synth.proc
import de.sciss.synth.proc.{Action, Obj}

object ActionView extends ListObjView.Factory with TimelineObjView.Factory {
  type E[S <: evt.Sys[S]] = Action.Elem[S]
  val icon        = ObjViewImpl.raphaelIcon(raphael.Shapes.Bolt)
  val prefix      = "Action"
  def typeID      = Action.typeID

  def mkListView[S <: Sys[S]](obj: Action.Obj[S])(implicit tx: S#Tx): ListObjView[S] =
    new ListImpl(tx.newHandle(obj), ObjViewImpl.nameOption(obj))

  type Config[S <: evt.Sys[S]] = String

  def hasMakeDialog   = true

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
      messageType = OptionPane.Message.Question, initial = prefix)
    opt.title = s"New $prefix"
    val res = opt.show(window)
    res
  }

  def makeObj[S <: Sys[S]](name: String)(implicit tx: S#Tx): List[Obj[S]] = {
    import proc.Implicits._
    val peer = Action.Var(Action.empty[S])
    val elem = Action.Elem(peer)
    val obj = Obj(elem)
    obj.name = name
    obj :: Nil
  }

  private trait Impl[S <: Sys[S]]
    extends ListObjView /* .Action */[S]
    with ActionView[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.NonEditable[S]
    with ListObjViewImpl.EmptyRenderer[S] {

    final type E[~ <: evt.Sys[~]] = Action.Elem[~]

    final def icon    = ActionView.icon
    final def prefix  = ActionView.prefix
    final def typeID  = ActionView.typeID

    final def isViewable = true

    final def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      import de.sciss.mellite.Mellite.compiler
      val frame = CodeFrame.action(obj())
      Some(frame)
    }

    final def action(implicit tx: S#Tx): Action.Obj[S] = obj()
  }

  private final class ListImpl[S <: Sys[S]](val obj: stm.Source[S#Tx, Action.Obj[S]],
                                var nameOption: Option[String])
    extends Impl[S]

  def mkTimelineView[S <: Sys[S]](id: S#ID, span: Expr[S, SpanLike], obj: Action.Obj[S],
                                  context: TimelineObjView.Context[S])(implicit tx: S#Tx): TimelineObjView[S] = {
    implicit val spanLikeSer = SpanLikeEx.serializer[S]
    val res = new TimelineImpl(tx.newHandle(span), tx.newHandle(obj))
    TimelineObjViewImpl.initAttrs    (span, obj, res)
    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
    res
  }

  private final class TimelineImpl[S <: Sys[S]](val span: Source[S#Tx, Expr[S, SpanLike]],
                                                val obj: stm.Source[S#Tx, Action.Obj[S]])
    extends Impl[S]
    with TimelineObjViewImpl.BasicImpl[S]
    with TimelineObjView.HasMute {

    var muted: Boolean = _
  }
}
trait ActionView[S <: evt.Sys[S]] extends ObjView[S] {
  override def obj: stm.Source[S#Tx, Action.Obj[S]]

  /** Convenience for `obj()` */
  def action(implicit tx: S#Tx): Action.Obj[S]
}