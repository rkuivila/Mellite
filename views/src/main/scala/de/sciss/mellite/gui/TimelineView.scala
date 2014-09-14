/*
 *  TimelineView.scala
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

import de.sciss.desktop.UndoManager
import de.sciss.lucre.swing.View
import de.sciss.synth.proc.{Timeline, Obj}
import scala.swing.{Component, Action}
import de.sciss.mellite.gui.impl.timeline.{TimelineViewImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.Sys

object TimelineView {
  def apply[S <: Sys[S]](group: Timeline.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): TimelineView[S] =
    Impl[S](group)

  final val TrackScale = 16
}
trait TimelineView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S] {
  def timelineModel   : TimelineModel
  def selectionModel  : TimelineObjView.SelectionModel[S]

  def group(implicit tx: S#Tx): Timeline.Obj[S]

  def canvasComponent: Component

  // ---- GUI actions ----
  def bounceAction      : Action
  def deleteAction      : Action
  def splitObjectsAction: Action
  def stopAllSoundAction: Action
}