package de.sciss
package mellite
package gui
package impl

import de.sciss.span.Span
import de.sciss.synth.expr.{ExprImplicits, Longs, Ints, Spans}
import de.sciss.synth.proc.{Sys, graph, Scan, Grapheme, Attribute, Proc}
import de.sciss.lucre.bitemp.{BiGroup, BiExpr}
import de.sciss.synth.SynthGraph

object DropAudioRegionAction {
  def apply[S <: Sys[S]](group: BiGroup.Modifiable[S, Proc[S], Proc.Update[S]], drop: AudioFileDnD.Drop[S])
                        (implicit tx: S#Tx) {
    val expr    = ExprImplicits[S]
    import expr._

    // val elem    = data.source()
    // val elemG = elem.entity
    val time    = drop.frame
    val sel     = drop.drag.selection
    val spanV   = Span(time, time + sel.length)
    val span    = Spans.newVar[S](spanV)
    val proc    = Proc[S]
    // proc.name_=(elem.name)
    val attr    = proc.attributes
    val track   = drop.y / 32
    attr.put(ProcKeys.attrTrack, Attribute.Int(Ints.newVar(track)))
    drop.drag.bus.foreach { busSource =>
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
    val gElem   = drop.drag.source().entity  // could there be a Grapheme.Element.Var?
    val bi: Grapheme.TimedElem[S] = BiExpr(gStart, gElem)
    grw.add(bi)
    // val gv = Grapheme.Value.Curve
    // val crv = gv(dur -> stepShape)
    // grd.add(time -> crv)
    scanw.source_=(Some(Scan.Link.Grapheme(grw)))
    // scand.source_=(Some(Scan.Link.Grapheme(grd)))
    val sg = SynthGraph {
      import synth._
      import ugen._
      val sig   = graph.scan     (ProcKeys.graphAudio).ar(0)
      val bus   = graph.attribute(ProcKeys.attrBus   ).ir(0)
      // val amp   = graph.attribute(ProcKeys.attrGain  ).ir(1)
      val mute  = graph.attribute(ProcKeys.attrMute  ).ir(0)
      // val env   = EnvGen.ar(Env.linen(0.2, (duri - 0.4).max(0), 0.2))
      val env   = /* amp * */ (1 - mute)
      Out.ar(bus, sig * env)
    }
    proc.graph_=(sg)
    group.add(span, proc)
  }
}