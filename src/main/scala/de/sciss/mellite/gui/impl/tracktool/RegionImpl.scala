/*
 *  RegionImpl.scala
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
package tracktool

import de.sciss.synth.proc.{Proc, Sys}
import java.awt.event.MouseEvent
import de.sciss.span.SpanLike
import de.sciss.lucre.expr.Expr

trait RegionImpl[S <: Sys[S], A] extends RegionLike[S, A] {
  tool =>

  protected def handlePress(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[timeline.ProcView[S]]): Unit = {
    handleMouseSelection(e, regionOpt)
    // now go on if region is selected
    regionOpt.foreach { region =>
      if (selm.contains(region)) handleSelect(e, hitTrack, pos, region)
    }
  }

  def commit(drag: A)(implicit tx: S#Tx): Unit =
    canvas.selectionModel.iterator.foreach { pv =>
      val span  = pv.spanSource()
      val proc  = pv.procSource()
      commitProc(drag)(span, proc)
    }

  protected def commitProc(drag: A)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: timeline.ProcView[S]): Unit
}