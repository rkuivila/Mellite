/*
 *  CollectionFrameImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
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
package component

import javax.swing.undo.UndoableEdit

import de.sciss.swingplus.PopupMenu

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.icons.raphael
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.Obj

import scala.swing.{Component, FlowPanel, Action, Button, BorderPanel}

object CollectionViewImpl {
  sealed trait Update
  final case class NamedChanged(name: String) extends Update

  //  def apply[S <: Sys[S], S1 <: Sys[S1]](peer: View.Editable[S])(implicit tx: S#Tx): CollectionsViewImpl[S, S1] = {
  //
  //  }
}
trait CollectionViewImpl[S <: Sys[S], S1 <: Sys[S1]]
  extends ViewHasWorkspace[S]
  with View.Editable[S]
  with ComponentHolder[Component]
  with ModelImpl[CollectionViewImpl.Update] {

  impl =>

  // ---- abstract ----

  protected def peer: View.Editable[S]

  protected def bridge: S#Tx => S1#Tx

  protected def nameObserver: stm.Disposable[S1#Tx]
  protected def mkTitle(sOpt: Option[String]): String

  protected def actionDelete: Action

  protected def selectedObjects: List[ObjView[S]]

  /** Called after the main GUI has been initialized. */
  protected def initGUI2(): Unit

  protected type InsertConfig

  protected def prepareInsert(f: ObjView.Factory): Option[InsertConfig]

  protected def editInsert(f: ObjView.Factory, xs: List[Obj[S]], config: InsertConfig)(implicit tx: S#Tx): Option[UndoableEdit]

  // ---- implemented ----

  lazy final protected val actionAttr: Action = Action(null) {
    val sel = selectedObjects
    if (sel.nonEmpty) cursor.step { implicit tx =>
      sel.foreach(n => AttrMapFrame(n.obj()))
    }
  }

  lazy final protected val actionView: Action = Action(null) {
    val sel = selectedObjects.filter(_.isViewable)
    if (sel.nonEmpty) cursor.step { implicit tx =>
      sel.foreach(_.openView())
    }
  }

  protected def selectionChanged(sel: List[ObjView[S]]): Unit = {
    val nonEmpty  = sel.nonEmpty
    actionAdd   .enabled  = sel.size < 2
    actionDelete.enabled  = nonEmpty
    actionView  .enabled  = nonEmpty && sel.exists(_.isViewable)
    actionAttr  .enabled  = nonEmpty
  }

  final protected var ggAdd   : Button = _
  final protected var ggDelete: Button = _
  final protected var ggView  : Button = _
  final protected var ggAttr  : Button = _

  final def init()(implicit tx: S#Tx): this.type = {
    deferTx(guiInit())
    this
  }

  private var _title: String = ""

  def title: String = _title

  final def nameUpdate(sOpt: Option[String]): Unit = {
    _title = mkTitle(sOpt)
    dispatch(CollectionViewImpl.NamedChanged(_title))
  }

  private final class AddAction(f: ObjView.Factory) extends Action(f.prefix) {
    icon = f.icon

    def apply(): Unit = {
      val winOpt    = GUI.findWindow(component)
      val confOpt   = f.initDialog[S](workspace, /* parentH, */ winOpt)
      confOpt.foreach { conf =>
        val confOpt2  = prepareInsert(f)
        confOpt2.foreach { insConf =>
          val editOpt = cursor.step { implicit tx =>
            val xs = f.make(conf)
            editInsert(f, xs, insConf)
          }
          editOpt.foreach(undoManager.add)
        }
      }
    }
  }

  private lazy val addPopup: PopupMenu = {
    import de.sciss.desktop.Menu._
    val pop = Popup()
    ObjView.factories.toList.sortBy(_.prefix).foreach { f =>
      pop.add(Item(f.prefix, new AddAction(f)))
    }

    val window = GUI.findWindow(component).getOrElse(sys.error(s"No window for $impl"))
    val res = pop.create(window)
    res.peer.pack() // so we can read `size` correctly
    res
  }

  final protected lazy val actionAdd: Action = Action(null) {
    val bp = ggAdd
    addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
  }

  private def guiInit(): Unit = {
    ggAdd     = GUI.toolButton(actionAdd   , raphael.Shapes.Plus  , "Add Element")
    ggDelete  = GUI.toolButton(actionDelete, raphael.Shapes.Minus , "Remove Selected Element")
    ggAttr    = GUI.toolButton(actionAttr  , raphael.Shapes.Wrench, "Attributes Editor")
    ggView    = GUI.toolButton(actionView  , raphael.Shapes.View  , "View Selected Element")

    val buttonPanel = new FlowPanel(ggAdd, ggDelete, ggAttr, ggView)

    component = new BorderPanel {
      add(impl.peer.component, BorderPanel.Position.Center)
      add(buttonPanel, BorderPanel.Position.South)
    }

    initGUI2()
    selectionChanged(selectedObjects)
  }

  def dispose()(implicit tx: S#Tx): Unit =
    nameObserver.dispose()(bridge(tx))
}

class CollectionFrameImpl[S <: Sys[S]](val view: View[S])
  extends WindowImpl[S] {
  impl =>
}