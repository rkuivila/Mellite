/*
 *  TimelineFrame.scala
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

package de.sciss
package mellite
package gui

import desktop.Window
import de.sciss.synth.proc.{ProcGroupElem, Obj}
import impl.timeline.{FrameImpl => Impl}
import lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.View

object TimelineFrame {
  def apply[S <: Sys[S]](document: Document[S], group: Obj.T[S, ProcGroupElem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): TimelineFrame[S] =
    Impl(document, group)
}
trait TimelineFrame[S <: Sys[S]] extends View[S] {
  def window  : Window
  def contents: TimelineView[S]
}