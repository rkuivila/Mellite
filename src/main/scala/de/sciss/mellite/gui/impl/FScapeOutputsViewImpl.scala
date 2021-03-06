/*
 *  FScapeOutputsViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui.impl

import java.awt.datatransfer.Transferable
import javax.swing.{DefaultListCellRenderer, Icon, JList, ListCellRenderer}

import de.sciss.desktop.{OptionPane, UndoManager, Window}
import de.sciss.equal.Implicits._
import de.sciss.fscape.lucre.FScape
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.{EditAddFScapeOutput, EditRemoveFScapeOutput}
import de.sciss.mellite.gui.impl.component.DragSourceButton
import de.sciss.mellite.gui.{DragAndDrop, FScapeOutputsView, GUI, ListObjView, MapView}
import de.sciss.swingplus.{ComboBox, ListView}
import de.sciss.synth.proc.Workspace

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.swing.Swing.HGlue
import scala.swing.{Action, BoxPanel, Button, Component, FlowPanel, Label, Orientation, ScrollPane, TextField}

object FScapeOutputsViewImpl {
  def apply[S <: Sys[S]](obj: FScape[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                       workspace: Workspace[S], undoManager: UndoManager): FScapeOutputsView[S] = {
    val list0 = obj.outputs.iterator.map { out =>
      (out.key, ListObjView(out))
    }  .toIndexedSeq

    new Impl(list0, tx.newHandle(obj)) {
      protected val observer: Disposable[S#Tx] = obj.changed.react { implicit tx =>upd =>
        upd.changes.foreach {
          case FScape.OutputAdded  (out) => attrAdded(out.key, out)
          case FScape.OutputRemoved(out) => attrRemoved(out.key)
          case _ => // graph change
        }
      }

      deferTx(guiInit())
    }
  }

  private abstract class Impl[S <: Sys[S]](list0: Vec[(String, ListObjView[S])],
                                           objH: stm.Source[S#Tx, FScape[S]])
                                          (implicit cursor: stm.Cursor[S], workspace: Workspace[S],
                                           undoManager: UndoManager)
    extends MapViewImpl[S, FScapeOutputsView[S]](list0) with FScapeOutputsView[S] with ComponentHolder[Component] { impl =>

    protected final def editRenameKey(before: String, now: String, value: Obj[S])(implicit tx: S#Tx) = None
    protected final def editImport(key: String, value: Obj[S], isInsert: Boolean)(implicit tx: S#Tx) = None

    override protected def keyEditable: Boolean = false
    override protected def showKeyOnly: Boolean = true

    private object ActionAdd extends Action("Out") {
      def apply(): Unit = {
        val key0  = "out"
        val tpe   = s"${title}put"
        // val opt   = OptionPane.textInput(message = s"$tpe Name", initial = key0)
        val seqTpe: Seq[(Obj.Type, Icon, String)] = ListObjView.factories.map { fact =>
          (fact.tpe, fact.icon, fact.humanName)
        } (breakOut)
        val ggTpe = new ComboBox(seqTpe.sortBy(_._3))
        // mein Gott, was fuer ein Terror
        val rDef = (new DefaultListCellRenderer).asInstanceOf[ListCellRenderer[AnyRef]]
        ggTpe.renderer = ListView.Renderer.wrap(new ListCellRenderer[(Obj.Type, Icon, String)] {
          override def getListCellRendererComponent(list: JList[_ <: (Obj.Type, Icon, String)],
                                                    value: (Obj.Type, Icon, String), index: Int,
                                                    isSelected: Boolean,
                                                    cellHasFocus: Boolean): java.awt.Component = {
            val (_, icon, name) = value
            val res = rDef.getListCellRendererComponent(list, name.asInstanceOf[AnyRef], index, isSelected, cellHasFocus)
            res match {
              case lb: javax.swing.JLabel =>
                lb.setIcon(icon)
              case _ =>
            }
            res
          }
        })
        val ggName  = new TextField(16)
        ggName.text = key0
        val lbName = new Label(s"$tpe Name:")
        val pMsg  = new FlowPanel(lbName, ggName, ggTpe)
        val opt   = OptionPane(message = pMsg, optionType = OptionPane.Options.OkCancel)
        opt.title = s"Add $tpe"
        val res = opt.show(Window.find(component))
        if (res === OptionPane.Result.Ok) {
          val tpe  = ggTpe.selection.item._1
          val key  = ggName.text
          val edit = cursor.step { implicit tx =>
            EditAddFScapeOutput(objH(), key = key, tpe = tpe)
          }
          undoManager.add(edit)
        }
      }
    }

    private lazy val actionRemove = Action(null) {
      selection.headOption.foreach { case (key, _ /* view */) =>
        val editOpt = cursor.step { implicit tx =>
          //          val obj     = objH()
//          val edits3: List[UndoableEdit] = Nil
          //            outputs.get(key).fold(List.empty[UndoableEdit]) { thisOutput =>
          //            val edits1 = thisOutput.iterator.toList.collect {
          //              case Output.Link.Output(thatOutput) =>
          //                val source  = thisOutput
          //                val sink    = thatOutput
          //                EditRemoveOutputLink(source = source, sink = sink)
          //            }
          //            edits1
          //          }
          // edits3.foreach(e => println(e.getPresentationName))
          val obj = objH()
          obj.outputs.get(key).map { output =>
            EditRemoveFScapeOutput(output)
          }
//          CompoundEdit(edits3 :+ editMain, "Remove Output")
        }
        editOpt.foreach(undoManager.add)
      }
    }

    private var ggDrag: Button = _

    private def selectionUpdated(): Unit = {
      val enabled = table.selection.rows.nonEmpty // .pages.nonEmpty
      actionRemove.enabled  = enabled
      ggDrag      .enabled  = enabled
    }

    final protected def guiInit1(scroll: ScrollPane): Unit = {
      // tab.preferredSize = (400, 100)
      val ggAddOut  = GUI.toolButton(ActionAdd   , raphael.Shapes.Plus , "Add Output"   )
      val ggDelete  = GUI.toolButton(actionRemove, raphael.Shapes.Minus, "Remove Output")
      ggDrag        = new DragSourceButton() {
        protected def createTransferable(): Option[Transferable] =
          selection.headOption.map { case (key, _ /* view */) =>
            DragAndDrop.Transferable(FScapeOutputsView.flavor)(FScapeOutputsView.Drag[S](
              workspace, objH, key))
          }
      }

      selectionUpdated()
      addListener {
        case MapView.SelectionChanged(_, _) => selectionUpdated()
      }

      val box = new BoxPanel(Orientation.Vertical) {
        contents += scroll // tab
        contents += new FlowPanel(ggAddOut, ggDelete, ggDrag, HGlue)
      }
      box.preferredSize = box.minimumSize
      component = box
    }
  }
}