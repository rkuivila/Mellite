/*
 *  ProcGUIActions.scala
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

import javax.swing.undo.UndoableEdit

import de.sciss.desktop.edit.CompoundEdit
import de.sciss.lucre.stm
import de.sciss.lucre.swing.requireEDT
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.{EditTimelineRemoveObj, Edits}
import de.sciss.synth.proc.{Proc, Timeline}

import scala.collection.immutable.{IndexedSeq => Vec}

/** These actions require being executed on the EDT. */
object ProcGUIActions {
  // scalac still has bug finding Timeline.Modifiable
  private type TimelineMod[S <: Sys[S]] = Timeline.Modifiable[S] // , Proc[S], Obj.UpdateT[S, Proc.Elem[S]]]

  def removeProcs[S <: Sys[S]](group: TimelineMod[S], views: TraversableOnce[TimelineObjView[S]])
                              (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    requireEDT()
    val edits = views.flatMap { pv0 =>
      val span  = pv0.span
      val obj   = pv0.obj

      val editsUnlink: Vec[UndoableEdit] = (pv0, obj) match {
        case (pv: ProcObjView.Timeline[S], procObj: Proc[S]) =>
          val thisProc  = procObj
          val edits     = Vector.newBuilder[UndoableEdit]

          def deleteLinks[A](isInput: Boolean): Unit = {
            val map = if (isInput) pv.inputs else pv.outputs
            map.foreach { case (thisKey, links) =>
              links.foreach{ case ProcObjView.Link(thatView, thatKey) =>
                val thisScans = if (isInput) thisProc.inputs else thisProc.outputs
                val thatProc  = thatView.obj
                val thatScans = if (isInput) thatProc.outputs else thatProc.inputs
                thisScans.get(thisKey).foreach { thisScan =>
                  thatScans.get(thatKey).foreach { thatScan =>
                    val source = if (isInput) thatScan else thisScan
                    val sink   = if (isInput) thisScan else thatScan
                    edits += Edits.removeLink(source = source, sink = sink)
                  }
                }
              }
            }
          }

          deleteLinks(isInput = true )
          deleteLinks(isInput = false)

          edits.result()

        case _ => Vector.empty
      }

      // group.remove(span, obj)
      editsUnlink :+ EditTimelineRemoveObj("Object", group, span, obj)
    } .toList

    CompoundEdit(edits, "Remove Object")
  }
}
