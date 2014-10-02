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

import scala.swing.{Component, FlowPanel, Action, Button, BorderPanel}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.icons.raphael
import de.sciss.lucre.swing.{View, deferTx}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.model.impl.ModelImpl

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

  protected def actionAdd   : Action
  protected def actionDelete: Action
  // protected def actionView  : Action
  // protected def actionAttr  : Action

  protected def selectedObjects: List[ObjView[S]]

  protected def initGUI2(): Unit

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

  private def guiInit(): Unit = {
    //    lazy val addPopup: PopupMenu = {
    //      import Menu._
    //      val pop = Popup()
    //      ObjView.factories.toList.sortBy(_.prefix).foreach { f =>
    //        pop.add(Item(f.prefix, new AddAction(f)))
    //      }
    //      val res = pop.create(window)
    //      res.peer.pack() // so we can read `size` correctly
    //      res
    //    }
    //
    //    lazy val actionAdd: Action = Action(null) {
    //      val bp = ggAdd
    //      addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
    //    }

    //    val actionDelete = Action(null) {
    //      val sel = contents.selection
    //      val edits: List[UndoableEdit] = atomic { implicit tx =>
    //        sel.map { nodeView =>
    //          val parent = nodeView.parentOption.flatMap { pView =>
    //            pView.modelData() match {
    //              case FolderElem.Obj(objT) => Some(objT.elem.peer)
    //              case _ => None
    //            }
    //          }.getOrElse(contents.root())
    //          val childH  = nodeView.modelData
    //          val child   = childH()
    //          val idx     = parent.indexOf(child)
    //          implicit val folderSer = Folder.serializer[S]
    //          val parentH = tx.newHandle(parent)
    //          EditRemoveObj[S](nodeView.renderData.prefix, parentH, idx, childH)
    //        }
    //      }
    //      edits match {
    //        case single :: Nil => undoManager.add(single)
    //        case Nil =>
    //        case several =>
    //          val ce = new CompoundEdit
    //          several.foreach(ce.addEdit)
    //          ce.end()
    //          undoManager.add(ce)
    //      }
    //    }

    ggAdd     = GUI.toolButton(actionAdd, raphael.Shapes.Plus, "Add Element")
    ggDelete  = GUI.toolButton(actionDelete, raphael.Shapes.Minus, "Remove Selected Element")
    ggAttr    = GUI.toolButton(actionAttr, raphael.Shapes.Wrench, "Attributes Editor")
    ggView    = GUI.toolButton(actionView, raphael.Shapes.View, "View Selected Element")

    val buttonPanel = new FlowPanel(ggAdd, ggDelete, ggAttr, ggView)

    component = new BorderPanel {
      add(impl.peer.component, BorderPanel.Position.Center)
      add(buttonPanel, BorderPanel.Position.South)
    }

    initGUI2()
    selectionChanged(selectedObjects)
  }

  def dispose()(implicit tx: S#Tx): Unit = {
    nameObserver.dispose()(bridge(tx))
  }
}

class CollectionFrameImpl[S <: Sys[S]](val view: View[S] /* CollectionViewImpl[S, _] */)
  extends WindowImpl[S] /* with WindowHolder[CollectionFrameImplPeer[S]] with CursorHolder[S] */ {
  impl =>

  //  final def component: Component = contents.component
  //
  //  final def dispose()(implicit tx: S#Tx): Unit = {
  //    disposeData()
  //    deferTx(window.dispose())
  //  }

  //  final protected def nameUpdate(name: Option[String]): Unit = {
  //    requireEDT()
  //    // window.title = mkTitle(name)
  //    title = mkTitle(name)
  //  }

  //  private def disposeData()(implicit tx: S#Tx): Unit = {
  //    contents  .dispose()
  //  }
  //
  //  final def frameClosing(): Unit =
  //    cursor.step { implicit tx =>
  //      disposeData()
  //    }

  //  private final class AddAction(f: ObjView.Factory) extends Action(f.prefix) {
  //    icon = f.icon
  //
  //    def apply(): Unit = {
  //      implicit val folderSer = Folder.serializer[S]
  //      val parentH = cursor.step { implicit tx => tx.newHandle(contents.insertionPoint._1) }
  //      f.initDialog[S](parentH, Some(window)).foreach(undoManager.add)
  //    }
  //  }

  //    window = new CollectionFrameImplPeer(this, compoundPanel, _fileOpt = file)
  //
  //    //    contents.addListener {
  //    //      case FolderView.SelectionChanged(_, sel) =>
  //    //        val nonEmpty = sel.nonEmpty
  //    //        actionAdd   .enabled  = sel.size < 2
  //    //        actionDelete.enabled  = nonEmpty
  //    //        actionView  .enabled  = nonEmpty && sel.exists(_.renderData.isViewable)
  //    //        actionAttr  .enabled  = nonEmpty
  //    //    }
  //
  //    initGUI2()
  //
  //    selectionChanged(selectedObjects)
  //    window.pack()
  //    GUI.placeWindow(window, frameX, frameY, 24)
}

//final class CollectionFrameImplPeer[S <: Sys[S]](view: CollectionFrameImpl[S, _], _contents: Component,
//                                                 _fileOpt: Option[File])
//  extends WindowImpl {
//
//  file            = _fileOpt
//  closeOperation  = Window.CloseDispose
//  reactions += {
//    case Window.Closing(_) => view.frameClosing()
//  }
//
//  bindMenus(
//    "edit.undo" -> view.contents.undoManager.undoAction,
//    "edit.redo" -> view.contents.undoManager.redoAction
//  )
//
//  contents = _contents
//
//  def show[A](source: DialogSource[A]): A = showDialog(source)
//}
