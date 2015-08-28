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
import de.sciss.lucre.bitemp.{SpanLike => SpanLikeObj}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.Source
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.{expr, Sys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.impl.timeline.TimelineObjViewImpl
import de.sciss.serial.Serializer
import de.sciss.span.SpanLike
import de.sciss.synth.proc
import de.sciss.synth.proc.{Action, Obj}

object ActionView extends ListObjView.Factory with TimelineObjView.Factory {
  type E[S <: stm.Sys[S]] = Action.Elem[S]
  val icon        = ObjViewImpl.raphaelIcon(raphael.Shapes.Bolt)
  val prefix      = "Action"
  def humanName   = prefix
  def typeID      = Action.typeID

  def category    = ObjView.categComposition

  def mkListView[S <: Sys[S]](obj: Action[S])(implicit tx: S#Tx): ListObjView[S] =
    new ListImpl(tx.newHandle(obj)).initAttrs(obj)

  type Config[S <: stm.Sys[S]] = String

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
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.NonEditable[S]
    with ListObjViewImpl.EmptyRenderer[S]
    with ActionView[S] {

    override def objH: stm.Source[S#Tx, Action[S]]

    override def obj(implicit tx: S#Tx): Action[S] = objH()

    final type E[~ <: stm.Sys[~]] = Action.Elem[~]

    final def factory = ActionView

    final def isViewable = true

    final def openView(parent: Option[Window[S]])
                      (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      import de.sciss.mellite.Mellite.compiler
      val frame = CodeFrame.action(obj)
      Some(frame)
    }
  }

  private final class ListImpl[S <: Sys[S]](val objH: stm.Source[S#Tx, Action[S]])
    extends Impl[S]

  def mkTimelineView[S <: Sys[S]](id: S#ID, span: SpanLikeObj[S], obj: Action[S],
                                  context: TimelineObjView.Context[S])(implicit tx: S#Tx): TimelineObjView[S] = {
    val res = new TimelineImpl(tx.newHandle(obj)).initAttrs(id, span, obj)
    TimelineObjViewImpl.initMuteAttrs(span, obj, res)
    res
  }

  private final class TimelineImpl[S <: Sys[S]](val objH : stm.Source[S#Tx, Action[S]])
    extends Impl[S]
    with TimelineObjViewImpl.BasicImpl[S]
    with TimelineObjView.HasMute {

    var muted: Boolean = _
  }
}
trait ActionView[S <: stm.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): Action[S]
}