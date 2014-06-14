/*
 *  AttrMapFrameImpl.scala
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

package de.sciss.mellite.gui
package impl
package document

import javax.swing.undo.UndoableEdit

import de.sciss.synth.proc
import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm

import scala.swing.Action
import de.sciss.mellite.Workspace
import de.sciss.mellite.gui.impl.component.{CollectionViewImpl, CollectionFrameImpl}
import de.sciss.lucre.stm.Disposable
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.{CompoundEdit, EditAttrMap}
import de.sciss.file._
import proc.Implicits._

object AttrMapFrameImpl {
  def apply[S <: Sys[S]](workspace: Workspace[S], obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): AttrMapFrame[S] = {
    implicit val undoMgr  = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val contents  = AttrMapView[S](workspace, obj)
    val view      = new ViewImpl[S](contents) {
      protected def nameObserver: Option[Disposable[S#Tx]] = None
    }
    view.init()
    val name      = obj.attr.name
    val res       = new FrameImpl[S](tx.newHandle(obj), view, title0 = s"$name : Attributes")
    res.init()
    res
  }

  private abstract class ViewImpl[S <: Sys[S]](val peer: AttrMapView[S])
                                              (implicit val cursor: stm.Cursor[S], val undoManager: UndoManager)
    extends CollectionViewImpl[S, S] {

    impl =>

    def workspace = peer.workspace

    protected val bridge: S#Tx => S#Tx = identity

    protected def nameObserver: Option[stm.Disposable[S#Tx]]

    protected def mkTitle(sOpt: Option[String]): String =
      s"${workspace.folder.base}${sOpt.fold("")(s => s"/$s")} : Attributes"

  //    private final class AddAction(f: ObjView.Factory) extends Action(f.prefix) {
  //      icon = f.icon
  //
  //      def apply(): Unit = {
  //        implicit val folderSer = Folder.serializer[S]
  //        val parentH = cursor.step { implicit tx => tx.newHandle(impl.peer.insertionPoint._1) }
  //        f.initDialog[S](parentH, None /* XXX TODO: Some(window) */).foreach(undoManager.add)
  //      }
  //    }

    //    private lazy val addPopup: PopupMenu = {
    //      import Menu._
    //      val pop = Popup()
    //      ObjView.factories.toList.sortBy(_.prefix).foreach { f =>
    //        pop.add(Item(f.prefix, new AddAction(f)))
    //      }
    //
    //      val window = GUI.findWindow(component).getOrElse(sys.error(s"No window for $impl"))
    //      val res = pop.create(window)
    //      res.peer.pack() // so we can read `size` correctly
    //      res
    //    }

    final protected lazy val actionAdd: Action = Action(null) {
      println("TODO: Add")
//      val bp = ggAdd
//      addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
    }

    final protected lazy val actionDelete: Action = Action(null) {
      val sel = peer.selection
      val edits: List[UndoableEdit] = cursor.step { implicit tx =>
        val obj0 = peer.obj
        sel.map { case (key, _) =>
          EditAttrMap(name = s"Delete Attribute '$key'", obj = obj0, key = key, value = None)
        }
      }
      val ceOpt = CompoundEdit(edits, "Delete Attributes")
      ceOpt.foreach(undoManager.add)
    }

    final protected def initGUI2(): Unit = {
      peer.addListener {
        case AttrMapView.SelectionChanged(_, sel) =>
          selectionChanged(sel.map(_._2))
      }
    }

    protected def selectedObjects: List[ObjView[S]] = peer.selection.map(_._2)
  }

  private final class FrameImpl[S <: Sys[S]](objH: stm.Source[S#Tx, Obj[S]], view: ViewImpl[S], title0: String)
                                       (implicit cursor: stm.Cursor[S], undoManager: UndoManager)
    extends CollectionFrameImpl[S](view)
    with AttrMapFrame[S] {

    def contents: AttrMapView[S] = view.peer

    def component = contents.component

    protected def nameObserver: Option[Disposable[S#Tx]] = None

    protected def mkTitle(sOpt: Option[String]): String = sOpt.getOrElse("<Untitled>")

    protected def selectedObjects: List[ObjView[S]] = contents.selection.map(_._2)

    protected lazy val actionAdd: Action = Action(null) {
      println("TODO: add")
    }

    protected lazy val actionDelete: Action = Action(null) {
      val sel = contents.selection
      if (sel.nonEmpty) {
        val editOpt = cursor.step { implicit tx =>
          val ed1 = sel.map { case (key, view1) =>
            EditAttrMap(name = s"Remove Attribute '$key'", objH(), key = key, value = None)
          }
          CompoundEdit(ed1, "Remove Attributes")
        }
        editOpt.foreach(undoManager.add)
      }
    }

    override protected def initGUI(): Unit = title = title0
  }
}