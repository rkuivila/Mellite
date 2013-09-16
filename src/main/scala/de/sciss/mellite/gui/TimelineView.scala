/*
 *  TimelineView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import de.sciss.synth.proc.AuralSystem
import scala.swing.{Action, Component}
import de.sciss.mellite.gui.impl.timeline.{TimelineViewImpl => Impl}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.Sys

object TimelineView {
  def apply[S <: Sys[S]](document: Document[S], group: Element.ProcGroup[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], aural: AuralSystem): TimelineView[S] =
    Impl(document, group)
}
trait TimelineView[S <: Sys[S]] extends Disposable[S#Tx] {
  def component         : Component
  def timelineModel     : TimelineModel
  def procSelectionModel: ProcSelectionModel[S]

  // ---- GUI actions ----
  def bounceAction      : Action
  def deleteAction      : Action
  def splitObjectsAction: Action
  def stopAllSoundAction: Action
}