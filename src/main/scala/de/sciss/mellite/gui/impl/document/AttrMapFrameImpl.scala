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

import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm

import scala.swing.Action
import de.sciss.mellite.Document
import de.sciss.mellite.gui.impl.component.CollectionFrameImpl
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.deferTx
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.UndoManager
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.{CompoundEdit, EditAttrMap}

object AttrMapFrameImpl {
  def apply[S <: Sys[S]](document: Document[S], obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): AttrMapFrame[S] = {
    implicit val undoMgr  = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val contents  = AttrMapView(obj)
    val res       = new Impl(document, tx.newHandle(obj), contents)
    deferTx {
      res.guiInit()
      res.window.front()
    }
    res
  }

  private final class Impl[S <: Sys[S]](document: Document[S], objH: stm.Source[S#Tx, Obj[S]],
                                        val contents: AttrMapView[S])
                                       (implicit cursor: stm.Cursor[S], undoManager: UndoManager)
    extends CollectionFrameImpl[S, S](document)
    with AttrMapFrame[S] {

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
          val ed1 = sel.map { case (key, view) =>
            EditAttrMap(name = s"Remove Attribute '$key'", objH(), key = key, value = None)
          }
          CompoundEdit(ed1, "Remove Attributes")
        }
        editOpt.foreach(undoManager.add)
      }
    }

    protected def initGUI2(): Unit = {
      contents.addListener {
        case AttrMapView.SelectionChanged(_, sel) =>
          selectionChanged(sel.map(_._2))
      }
    }
  }
}