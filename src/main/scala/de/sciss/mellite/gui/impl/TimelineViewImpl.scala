/*
 *  TimelineViewImpl.scala
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
package impl

import de.sciss.synth.proc.Sys
import de.sciss.audiowidgets.{AxisFormat, Axis}
import swing.{Orientation, BoxPanel}

object TimelineViewImpl {
  def apply[S <: Sys[S]](group: Element.ProcGroup[S])(implicit tx: S#Tx): TimelineView[S] = {
    val res = new Impl[S]
    guiFromTx(res.guiInit())
    res
  }

  private final class Impl[S <: Sys[S]] extends TimelineView[S] {
    def guiInit() {
      component
    }

    lazy val timelineAxis = new Axis {
      format  = AxisFormat.Time(hours = true, millis = true)
      minimum = 0.0
      maximum = 60.0
    }

    lazy val component = new BoxPanel(Orientation.Vertical) {
      contents += timelineAxis
    }
  }
}