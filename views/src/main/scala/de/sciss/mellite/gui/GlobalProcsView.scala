/*
 *  GlobalProcsView.scala
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

import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ProcObjView
import de.sciss.mellite.gui.impl.timeline.{GlobalProcsViewImpl => Impl}
import de.sciss.synth.proc.Timeline

import scala.swing.{Component, Table}

object GlobalProcsView {
  def apply[S <: Sys[S]](group: Timeline[S], selectionModel: SelectionModel[S, TimelineObjView[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): GlobalProcsView[S] =
      Impl(group, selectionModel)
}
trait GlobalProcsView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Component
  def tableComponent: Table

  def selectionModel: SelectionModel[S, ProcObjView.Timeline[S]]

  def iterator: Iterator[ProcObjView.Timeline[S]]

  def add    (proc: ProcObjView.Timeline[S]): Unit
  def remove (proc: ProcObjView.Timeline[S]): Unit
  def updated(proc: ProcObjView.Timeline[S]): Unit
}