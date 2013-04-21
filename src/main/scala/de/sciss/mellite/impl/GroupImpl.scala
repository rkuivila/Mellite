//package de.sciss.mellite
//package impl
//
//import de.sciss.synth.proc.Sys
//import de.sciss.lucre.{DataOutput, stm, DataInput}
//import stm.Mutable
//
//object GroupImpl {
//  def apply[S <: Sys[S]](implicit tx: S#Tx): Group[S] = {
//    val id        = tx.newID()
//    val elements  = Elements[S]
//    new ActiveImpl(id, elements)
//  }
//
//  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Group[S] = {
//    val id        = tx.readID(in, access)
//    val elements  = Elements.read(in, access)
//    new ActiveImpl(id, elements)
//  }
//
//  private final class ActiveImpl[S <: Sys[S]](val id: S#ID, val elements: Elements[S])
//    extends Group[S] with Mutable.ActiveImpl[S] {
//
//    protected def disposeData()(implicit tx: S#Tx) {
//      elements.dispose()
//    }
//
//    protected def writeData(out: DataOutput) {
//      elements.write(out)
//    }
//  }
//}