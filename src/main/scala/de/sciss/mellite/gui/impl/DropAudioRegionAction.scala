package de.sciss
package mellite
package gui
package impl

import de.sciss.span.Span
import de.sciss.synth.expr.{SynthGraphs, ExprImplicits, Longs, Ints, Spans}
import de.sciss.synth.proc.{ProcKeys, Sys, graph, Scan, Grapheme, Attribute, Proc}
import de.sciss.lucre.bitemp.{BiGroup, BiExpr}
import de.sciss.synth.SynthGraph

object DropAudioRegionAction {
  def apply[S <: Sys[S]](group: BiGroup.Modifiable[S, Proc[S], Proc.Update[S]],
                         time: Long, track: Int, drag: TimelineDnD.AudioDrag[S])
                        (implicit tx: S#Tx) {
    val imp = ExprImplicits[S]
    import imp._

    // val elem    = data.source()
    // val elemG = elem.entity
    val sel     = drag.selection
    val spanV   = Span(time, time + sel.length)
    val span    = Spans.newVar[S](spanV)
    val proc    = Proc[S]
    // proc.name_=(elem.name)
    val attr    = proc.attributes
    attr.put(ProcKeys.attrTrack, Attribute.Int(Ints.newVar(track)))
    drag.bus.foreach { busSource =>
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
    val gStart  = Longs.newVar(time - sel.start)  // wooopa, could even be a bin op at some point
    val gElem   = drag.source().entity  // could there be a Grapheme.Element.Var?
    val bi: Grapheme.TimedElem[S] = BiExpr(gStart, gElem)
    grw.add(bi)
    // val gv = Grapheme.Value.Curve
    // val crv = gv(dur -> stepShape)
    // grd.add(time -> crv)
    scanw.source_=(Some(Scan.Link.Grapheme(grw)))
    proc.graph_=(SynthGraphs.tape)
    group.add(span, proc)
  }
}