/*
 *  TimelineFrame.scala
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

package de.sciss.mellite
package gui

import de.sciss.lucre
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.timeline.{TimelineFrameImpl => Impl}
import de.sciss.synth.proc.{Timeline, Workspace}

object TimelineFrame {
  def apply[S <: Sys[S]](group: Timeline[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): TimelineFrame[S] =
    Impl(group)
}
trait TimelineFrame[S <: Sys[S]] extends lucre.swing.Window[S] {
  def view: TimelineView[S]
}