/*
 *  ProcActions.scala
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

import lucre.expr.Expr
import span.{Span, SpanLike}
import de.sciss.synth.proc.{Attribute, ProcKeys, Scan, Grapheme, Sys, Proc}
import de.sciss.synth.expr.{SynthGraphs, Longs, Ints, Spans, ExprImplicits}
import de.sciss.lucre.bitemp.{BiGroup, BiExpr}
import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.stm

object ProcActions {
  private val MinDur    = 32

  final case class Resize(deltaStart: Long, deltaStop: Long)

  /** Queries the audio region's grapheme segment start and audio element. */
  def getAudioRegion[S <: Sys[S]](span: Expr[S, SpanLike], proc: Proc[S])
                                 (implicit tx: S#Tx): Option[(Expr[S, Long], Grapheme.Elem.Audio[S])] = {
    span.value match {
      case Span.HasStart(frame) =>
        for {
          scan <- proc.scans.get(ProcKeys.graphAudio)
          Scan.Link.Grapheme(g) <- scan.sources.toList.headOption
          BiExpr(time, audio: Grapheme.Elem.Audio[S]) <- g.at(frame)
        } yield (time, audio)

      case _ => None
    }
  }

  def resize[S <: Sys[S]](span: Expr[S, SpanLike], proc: Proc[S], amount: Resize, timelineModel: TimelineModel)
                         (implicit tx: S#Tx): Unit = {
    import amount._

    val oldSpan   = span.value
    val minStart  = timelineModel.bounds.start
    val dStartC   = if (deltaStart >= 0) deltaStart else oldSpan match {
      case Span.HasStart(oldStart)  => math.max(-(oldStart - minStart)         , deltaStart)
      case _ => 0L
    }
    val dStopC   = if (deltaStop >= 0) deltaStop else oldSpan match {
      case Span.HasStop (oldStop)   => math.max(-(oldStop  - minStart + MinDur), deltaStop)
      case _ => 0L
    }

    if (dStartC != 0L || dStopC != 0L) {
      val imp = ExprImplicits[S]
      import imp._

      val (dStartCC, dStopCC) = getAudioRegion(span, proc) match {
        //        case Some((gtime, audio)) => // audio region
        //          val gtimeV  = gtime.value
        //          val audioV  = audio.value
        //          dStartC

        case _ => // other proc
          (dStartC, dStopC)
      }

      span match {
        case Expr.Var(s) =>
          s.transform { oldSpan =>
            oldSpan.value match {
              case Span.From(start)   => Span.From(start + dStartCC)
              case Span.Until(stop)   => Span.Until(stop  + dStopCC)
              case Span(start, stop)  =>
                val newStart = start + dStartCC
                Span(newStart, math.max(newStart + MinDur, stop + dStopCC))
              case other => other
            }
          }
      }
    }
  }

  def insertAudioRegion[S <: Sys[S]](
      group     : BiGroup.Modifiable[S, Proc[S], Proc.Update[S]],
      time      : Long,
      track     : Int,
      grapheme  : Grapheme.Elem.Audio[S],
      selection : Span,
      bus       : Option[Expr[S, Int]]) // stm.Source[S#Tx, Element.Int[S]]])
     (implicit tx: S#Tx): (Expr[S, Span], Proc[S]) = {

    val imp = ExprImplicits[S]
    import imp._

    val spanV   = Span(time, time + selection.length)
    val span    = Spans.newVar[S](spanV)
    val proc    = Proc[S]
    val attr    = proc.attributes
    attr.put(ProcKeys.attrTrack, Attribute.Int(Ints.newVar(track)))
    bus.foreach { busEx =>
      val bus = Attribute.Int(busEx)
      attr.put(ProcKeys.attrBus, bus)
    }

    val scanIn  = proc.scans.add(ProcKeys.graphAudio)
    /* val scanOut = */ proc.scans.add(ProcKeys.scanMainOut)
    val grIn    = Grapheme.Modifiable[S]

    // we preserve data.source(), i.e. the original audio file offset
    // ; therefore the grapheme element must start `selection.start` frames
    // before the insertion position `drop.frame`
    val gStart  = Longs.newVar(time - selection.start)  // wooopa, could even be a bin op at some point
    val bi: Grapheme.TimedElem[S] = BiExpr(gStart, grapheme)
    grIn.add(bi)
    scanIn addSource grIn
    proc.graph() = SynthGraphs.tape
    group.add(span, proc)

    (span, proc)
  }
}