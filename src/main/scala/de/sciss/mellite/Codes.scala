package de.sciss.mellite

import de.sciss.synth.expr.BiTypeImpl
import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.serial.{DataOutput, DataInput}

object Codes extends BiTypeImpl[Code] {
  final val typeID = 0x20001

  def readValue(in: DataInput): Code = Code.read(in)
  def writeValue(value: Code, out: DataOutput): Unit = value.write(out)

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): Codes.ReprNode[S] = {
    sys.error(s"No tuple operations defind for Code ($cookie)")
  }
}