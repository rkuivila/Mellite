/*
 *  ElementsFrameImpl.scala
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
package document

import scala.swing.{ComboBox, TextField, Dialog, Component, FlowPanel, Action, Button, BorderPanel}
import de.sciss.lucre.stm
import de.sciss.synth.proc.{FolderElem, Elem, IntElem, DoubleElem, Obj, Folder, ExprImplicits, ProcGroup, StringElem, ProcGroupElem}
import de.sciss.desktop.{UndoManager, FileDialog, DialogSource, Window, Menu}
import de.sciss.synth.io.AudioFile
import javax.swing.SpinnerNumberModel
import de.sciss.file._
import de.sciss.swingplus.{PopupMenu, Spinner}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.{String => StringEx, Double => DoubleEx, Int => IntEx, Expr}
import scala.util.Try
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.synth.proc
import proc.Implicits._
import de.sciss.icons.raphael
import de.sciss.desktop.impl.UndoManagerImpl

object ElementsFrameImpl {
  def apply[S <: Sys[S], S1 <: Sys[S1]](doc: Document[S], nameOpt: Option[Expr[S1, String]])(implicit tx: S#Tx,
                                        cursor: stm.Cursor[S], bridge: S#Tx => S1#Tx): DocumentElementsFrame[S] = {
    implicit val undoMgr  = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val folderView      = FolderView(doc.folder, doc.root())
    val name0           = nameOpt.map(_.value(bridge(tx)))
    val view            = new Impl[S, S1](doc, folderView) {
      protected val nameObserver = nameOpt.map { name =>
        name.changed.react { implicit tx => upd =>
          deferTx(nameUpdate(Some(upd.now)))
        } (bridge(tx))
      }

      deferTx {
        guiInit()
        nameUpdate(name0)
        component.front()
      }
    }

    view
  }

  private abstract class Impl[S <: Sys[S], S1 <: Sys[S1]](val document: Document[S], val folderView: FolderView[S])
                                       (implicit val cursor: stm.Cursor[S], val undoManager: UndoManager,
                                        bridge: S#Tx => S1#Tx)
    extends DocumentElementsFrame[S] with ComponentHolder[Frame[S]] with CursorHolder[S] {

    // protected implicit def cursor: Cursor[S] = document.cursor

    protected def nameObserver: Option[stm.Disposable[S1#Tx]]

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      deferTx(component.dispose())
    }

    final protected def nameUpdate(name: Option[String]): Unit = {
      requireEDT()
      component.title = mkTitle(name)
    }

    private def mkTitle(sOpt: Option[String]) = s"${document.folder.base}${sOpt.fold("")(s => s"/$s")} : Elements"

    private def disposeData()(implicit tx: S#Tx): Unit = {
      folderView  .dispose()
      nameObserver.foreach(_.dispose()(bridge(tx)))
    }

    final def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    private final class AddAction(f: ObjView.Factory) extends Action(f.prefix) {
      icon = f.icon

      def apply(): Unit = {
        implicit val folderSer = Folder.serializer[S]
        val parentH = cursor.step { implicit tx => tx.newHandle(folderView.insertionPoint._1) }
        f.initDialog[S](parentH, Some(component)).foreach(undoManager.add)
      }
    }

    final protected def guiInit(): Unit = {
      lazy val addPopup: PopupMenu = {
        import Menu._
        val pop = Popup()
        ObjView.factories.toList.sortBy(_.prefix).foreach { f =>
          pop.add(Item(f.prefix, new AddAction(f)))
        }

//          .add(Item("folder",        Action("Folder"       )(actionAddFolder          ())))
//          .add(Item("proc-group",    Action("ProcGroup"    )(actionAddProcGroup       ())))
//          .add(Item("artifact-store",Action("ArtifactStore")(actionAddArtifactLocation())))
//          .add(Item("audio-file",    Action("Audio File"   )(actionAddAudioFile       ())))
//          .add(Item("string",        Action("String"       )(actionAddString          ())))
//          .add(Item("int",           Action("Int"          )(actionAddInt             ())))
//          .add(Item("double",        Action("Double"       )(actionAddDouble          ())))
//          .add(Item("code",          Action("Code"         )(actionAddCode            ())))
        val res = pop.create(component)
        res.peer.pack() // so we can read `size` correctly
        res
      }

      lazy val actionAdd: Action = Action(null) {
        val bp = ggAdd
        addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
      }

      lazy val ggAdd: Button = GUI.toolButton(actionAdd, raphael.Shapes.Plus, "Add Element")

      val actionDelete = Action(null) {
        ???
//        val views = folderView.selection.map { case (p, view) => (p.last, view) }
//        if (views.nonEmpty) atomic { implicit tx =>
//          views.foreach {
//            case (parent: ObjView.FolderLike[S], child) =>
//              parent.folder().remove(child.obj())
//            case _ =>
//          }
//        }
      }
      actionDelete.enabled = false

      lazy val ggDelete: Button = GUI.toolButton(actionDelete, raphael.Shapes.Minus, "Remove Selected Element")

      val actionView = Action(null) {
        ???
//        val views = folderView.selection.map { case (_, view) => view }
//        if (views.nonEmpty) atomic { implicit tx =>
//          import Mellite.auralSystem
//          views.foreach {
//            case view: ObjView.ProcGroup[S] =>
//              // val e   = view.element()
//              // import document.inMemory
//              TimelineFrame(document, view.obj())
//
//            case view: ObjView.AudioGrapheme[S] =>
//              AudioFileFrame(document, view.obj())
//
//            case view: ObjView.Recursion[S] =>
//              RecursionFrame(document, view.obj())
//
//            case view: ObjView.Code[S] =>
//              CodeFrame(document, view.obj())
//
//            case _ => // ...
//          }
//        }
      }
      actionView.enabled = false

      lazy val ggView: Button = GUI.toolButton(actionView, raphael.Shapes.View, "View Selected Element")

      lazy val folderButPanel = new FlowPanel(ggAdd, ggDelete, ggView)

      lazy val folderPanel = new BorderPanel {
        add(folderView.component, BorderPanel.Position.Center)
        add(folderButPanel,       BorderPanel.Position.South )
      }

      component = new Frame(this, folderPanel)

      folderView.addListener {
        case FolderView.SelectionChanged(_, sel) =>
          actionAdd   .enabled  = sel.size < 2
          actionDelete.enabled  = sel.nonEmpty
          ggView      .enabled  = sel.nonEmpty
      }
    }
  }

  private final class Frame[S <: Sys[S]](view: Impl[S, _], _contents: Component) extends WindowImpl {
    file            = Some(view.document.folder)
    closeOperation  = Window.CloseDispose
    reactions += {
      case Window.Closing(_) => view.frameClosing()
    }

    bindMenus(
      "edit.undo" -> view.folderView.undoManager.undoAction,
      "edit.redo" -> view.folderView.undoManager.redoAction
    )

    contents        = _contents

    pack()
    // centerOnScreen()
    GUI.placeWindow(this, 0.5f, 0f, 24)

    def show[A](source: DialogSource[A]): A = showDialog(source)
  }
}