/*
 *  MuteImpl.scala
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

import de.sciss.synth.proc.{ProcKeys, Attribute, Proc, Sys}
import de.sciss.model.impl.ModelImpl
import java.awt.{Point, Toolkit}
import java.awt.event.MouseEvent
import de.sciss.lucre.expr.Expr
import de.sciss.span.SpanLike
import de.sciss.synth.expr.Booleans
import de.sciss.mellite.gui.TrackTool.Mute
import de.sciss.mellite.gui.impl.timeline.TimelineProcView

object MuteImpl {
  private lazy val cursor = {
    val tk  = Toolkit.getDefaultToolkit
    val img = tk.createImage(Mellite.getClass.getResource("cursor-mute.png"))
    tk.createCustomCursor(img, new Point(4, 4), "Mute")
  }
}
final class MuteImpl[S <: Sys[S]](protected val canvas: TimelineProcCanvas[S])
  extends RegionImpl[S, Mute] with ModelImpl[TrackTool.Update[Mute]] {

  def defaultCursor = MuteImpl.cursor
  val name          = "Mute"
  val icon          = ToolsImpl.getIcon("mute")

  protected def commitProc(mute: Mute)(span: Expr[S, SpanLike], proc: Proc[S])(implicit tx: S#Tx): Unit = {
    val attr      = proc.attributes
    attr[Attribute.Boolean](ProcKeys.attrMute) match {
      // XXX TODO: Booleans should have `not` operator
      case Some(Expr.Var(vr)) => vr.transform { old => val vOld = old.value; Booleans.newConst(!vOld) }
      case _                  => attr.put(ProcKeys.attrMute, Attribute.Boolean(Booleans.newVar(Booleans.newConst(true))))
    }
  }

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView[S]): Unit =
    dispatch(TrackTool.Adjust(Mute(!region.muted)))
}
