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
import de.sciss.synth.proc.{Proc, Scan, Timeline}

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

/** These actions require being executed on the EDT. */
object ProcGUIActions {
  // scalac still has bug finding Timeline.Modifiable
  private type TimelineMod[S <: Sys[S]] = Timeline.Modifiable[S] // , Obj.T[S, Proc.Elem], Obj.UpdateT[S, Proc.Elem[S]]]

  def removeProcs[S <: Sys[S]](group: TimelineMod[S], views: TraversableOnce[TimelineObjView[S]])
                              (implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    requireEDT()
    val edits = views.flatMap { pv0 =>
      val span  = pv0.span
      val obj   = pv0.obj

      val editsUnlink: Vec[UndoableEdit] = (pv0, obj) match {
        case (pv: ProcObjView.Timeline[S], Proc.Obj(proc)) =>
          def deleteLinks[A](map: ProcObjView.LinkMap[S])(fun: (String, Scan[S], String, Scan[S]) => A): Vec[A] =
            map.flatMap { case (thisKey, links) =>
              links.flatMap { case ProcObjView.Link(thatView, thatKey) =>
                proc.elem.peer.scans.get(thisKey).flatMap { thisScan =>
                  thatView.obj.elem.peer.scans.get(thatKey).map { thatScan =>
                    fun(thisKey, thisScan, thatKey, thatScan)
                  }
                }
              } (breakOut)
            } (breakOut)

          val e1 = deleteLinks(pv.inputs) { (thisKey, thisScan, thatKey, thatScan) =>
            Edits.removeLink(/* sourceKey = thatKey, */ source = thatScan, /* sinkKey = thisKey, */ sink = thisScan)
          }
          val e2 = deleteLinks(pv.outputs) { (thisKey, thisScan, thatKey, thatScan) =>
            Edits.removeLink(/* sourceKey = thisKey, */ source = thisScan, /* sinkKey = thatKey, */ sink = thatScan)
          }

          e1 ++ e2

        case _ => Vector.empty
      }

      // group.remove(span, obj)
      editsUnlink :+ EditTimelineRemoveObj("Object", group, span, obj)
    } .toList

    CompoundEdit(edits, "Remove Object")
  }
}
