//package de.sciss.mellite
//
//import de.sciss.synth.proc.Sys
//import de.sciss.lucre.stm.Mutable
//import de.sciss.lucre.DataInput
//import impl.{GroupImpl => ActiveImpl}
//
//object Group {
//  final val typeID = 0x10000
//
//  def apply[S <: Sys[S]](implicit tx: S#Tx): Group[S] = ActiveImpl.read()
//  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Group[S] = ActiveImpl.read(in, access)
//}
//trait Group[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
//  def elements: Elements[S]
//}