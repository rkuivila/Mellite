package de.sciss.soundprocesses.tutorial

import de.sciss.synth.SynthGraph

trait Snippet1Parts {
  // #snippet1graph
  val bubbles = SynthGraph {
    import de.sciss.synth._
    import ugen._
    val o = LFSaw.kr(Seq(8, 7.23)).madd(3, 80)
    val f = LFSaw.kr(0.4).madd(24, o)
    val s = SinOsc.ar(f.midicps) * 0.04
    val c = CombN.ar(s, 0.2, 0.2, 4)
    val l = Line.kr(1, 0, 10, doneAction = freeSelf)
    Out.ar(0, c * l)
  }
  // #snippet1graph
}