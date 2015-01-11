/*
 *  TimelineFrame.scala
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

package de.sciss
package mellite
package gui

import de.sciss.synth.proc.{Timeline, Obj}
import impl.timeline.{FrameImpl => Impl}
import lucre.stm
import de.sciss.lucre.synth.Sys

object TimelineFrame {
  def apply[S <: Sys[S]](group: Timeline.Obj[S])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): TimelineFrame[S] =
    Impl(group)
}
trait TimelineFrame[S <: Sys[S]] extends lucre.swing.Window[S] {
  def view: TimelineView[S]
}