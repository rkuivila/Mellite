/*
 *  TimelineView.scala
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

import de.sciss.audiowidgets.TimelineModel
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.timeline.{TimelineViewImpl => Impl}
import de.sciss.synth.proc.Timeline
import de.sciss.synth.proc.gui.TransportView

import scala.swing.{Action, Component}

object TimelineView {
  def apply[S <: Sys[S]](group: Timeline.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): TimelineView[S] =
    Impl[S](group)

  /** Number of pixels for one unit of track height (convention). */
  final val TrackScale  = 16
  /** Minimum duration in sample-frames for some cases where it should be greater than zero. */
  final val MinDur      = 32
}
trait TimelineView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S] {
  def timelineModel   : TimelineModel
  def selectionModel  : TimelineObjView.SelectionModel[S]

  def timelineObjH: stm.Source[S#Tx, Timeline.Obj[S]]
  def timelineObj(implicit tx: S#Tx): Timeline.Obj[S]

  def canvasComponent: Component

  def globalView   : GlobalProcsView[S]
  def transportView: TransportView  [S]

  // ---- GUI actions ----
  def actionBounce              : Action
  def actionDelete              : Action
  def actionSplitObjects        : Action
  def actionStopAllSound        : Action
  def actionClearSpan           : Action
  def actionRemoveSpan          : Action
  def actionAlignObjectsToCursor: Action
}