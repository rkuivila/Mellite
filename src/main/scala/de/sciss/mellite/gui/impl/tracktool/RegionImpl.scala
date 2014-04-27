/*
 *  RegionImpl.scala
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
package impl
package tracktool

import de.sciss.synth.proc.{Obj, ProcElem, Proc}
import java.awt.event.MouseEvent
import de.sciss.span.SpanLike
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.synth.Sys

trait RegionImpl[S <: Sys[S], A] extends RegionLike[S, A] {
  tool =>

  protected def handlePress(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[timeline.ProcView[S]]): Unit = {
    handleMouseSelection(e, regionOpt)
    // now go on if region is selected
    regionOpt.foreach { region =>
      if (canvas.selectionModel.contains(region)) handleSelect(e, hitTrack, pos, region)
    }
  }

  def commit(drag: A)(implicit tx: S#Tx): Unit =
    canvas.selectionModel.iterator.foreach { pv =>
      val span  = pv.spanSource()
      val proc  = pv.procSource()
      commitProc(drag)(span, proc)
    }

  protected def commitProc(drag: A)(span: Expr[S, SpanLike], proc: Obj.T[S, ProcElem])(implicit tx: S#Tx): Unit

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: timeline.ProcView[S]): Unit
}