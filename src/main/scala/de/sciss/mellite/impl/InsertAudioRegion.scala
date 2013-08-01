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
    scanw.source_=(Some(Scan.Link.Grapheme(grw)))
    proc.graph() = SynthGraphs.tape
    group.add(span, proc)

    (span, proc)
  }
}