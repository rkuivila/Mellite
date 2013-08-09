package de.sciss.mellite

import de.sciss.synth.proc.Artifact.Location
import de.sciss.synth.proc.{Grapheme, Sys}
import de.sciss.synth.expr.{Doubles, Longs}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.file._

object ElementActions {
  def addAudioFile[S <: Sys[S]](folder: Folder[S], index: Int, loc: Location.Modifiable[S],
                                f: File, spec: AudioFileSpec)
                               (implicit tx: S#Tx): Unit = {
    val offset    = Longs  .newVar[S](Longs  .newConst(0L))
    val gain      = Doubles.newVar[S](Doubles.newConst(1.0))
    val artifact  = loc.add(f)
    val audio     = Grapheme.Elem.Audio(artifact, spec, offset, gain)
    val name      = f.base
    val elem      = Element.AudioGrapheme(name, audio)
    if (index == -1) folder.addLast(elem) else folder.insert(index, elem)
  }
}