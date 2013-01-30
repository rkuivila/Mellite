package de.sciss.mellite
package impl

import de.sciss.synth.proc.Sys
import de.sciss.lucre.{DataOutput, stm, DataInput}
import stm.Mutable

object GroupImpl {
  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Group[S] = {
    val id        = tx.newID()
    val elements  = Elements[S]
    new Impl(id, elements)
  }

  private final class Impl[S <: Sys[S]](val id: S#ID, val elements: Elements[S])
    extends Group[S] with Mutable.Impl[S] {

    protected def disposeData()(implicit tx: S#Tx) {
      elements.dispose()
    }

    protected def writeData(out: DataOutput) {
      elements.write(out)
    }
  }
}