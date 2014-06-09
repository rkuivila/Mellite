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

import de.sciss.synth.proc.{ProcGroupElem, ProcGroup, Obj}
import scala.swing.{Action, Component}
import de.sciss.mellite.gui.impl.timeline.{TimelineViewImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.Sys

object TimelineView {
  def apply[S <: Sys[S]](document: Workspace[S], group: Obj.T[S, ProcGroupElem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): TimelineView[S] =
    Impl[S](document, group)
}
trait TimelineView[S <: Sys[S]] extends DocumentView[S] {
  def component         : Component
  def timelineModel     : TimelineModel
  def procSelectionModel: ProcSelectionModel[S]

  def group(implicit tx: S#Tx): Obj.T[S, ProcGroupElem]

  // ---- GUI actions ----
  def bounceAction      : Action
  def deleteAction      : Action
  def splitObjectsAction: Action
  def stopAllSoundAction: Action
}