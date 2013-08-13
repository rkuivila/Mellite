/*
 *  InsertAudioRegion.scala
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

package de.sciss
package mellite
package impl

import span.Span
import synth.expr.{SynthGraphs, ExprImplicits, Longs, Ints, Spans}
import synth.proc.{ProcKeys, Sys, Scan, Grapheme, Attribute, Proc}
import de.sciss.lucre.bitemp.{BiGroup, BiExpr}
import de.sciss.lucre.stm
import de.sciss.lucre.expr.Expr

object InsertAudioRegion {
  def apply[S <: Sys[S]](group: BiGroup.Modifiable[S, Proc[S], Proc.Update[S]],
                         time: Long, track: Int, // drag: TimelineDnD.AudioDrag[S])
                         grapheme: Grapheme.Elem.Audio[S],
                         selection: Span, bus: Option[stm.Source[S#Tx, Element.Int[S]]])
                        (implicit tx: S#Tx): (Expr[S, Span], Proc[S]) = {
    val imp = ExprImplicits[S]
    import imp._

    // val elem    = data.source()
    // val elemG = elem.entity
    val spanV   = Span(time, time + selection.length)
    val span    = Spans.newVar[S](spanV)
    val proc    = Proc[S]
    // proc.name_=(elem.name)
    val attr    = proc.attributes
    attr.put(ProcKeys.attrTrack, Attribute.Int(Ints.newVar(track)))
    bus.foreach { busSource =>
      // val busName = busSource().name.value
      val bus = Attribute.Int(busSource().entity)
      attr.put(ProcKeys.attrBus, bus)
    }

    val scanw   = proc.scans.add(ProcKeys.graphAudio)
    // val scand   = proc.scans.add("dur")
    val grw     = Grapheme.Modifiable[S]
    // val grd     = Grapheme.Modifiable[S]

    // we preserve data.source(), i.e. the original audio file offset
    // ; therefore the grapheme element must start `selection.start` frames
    // before the insertion position `drop.frame`
    val gStart  = Longs.newVar(time - selection.start)  // wooopa, could even be a bin op at some point
    val bi: Grapheme.TimedElem[S] = BiExpr(gStart, grapheme)
    grw.add(bi)
    // val gv = Grapheme.Value.Curve
    // val crv = gv(dur -> stepShape)
    // grd.add(time -> crv)
    scanw addSource grw
    proc.graph() = SynthGraphs.tape
    group.add(span, proc)

    (span, proc)
  }
}