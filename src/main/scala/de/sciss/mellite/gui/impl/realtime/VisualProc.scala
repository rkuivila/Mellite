package de.sciss.mellite.gui.impl.realtime

import de.sciss.synth.proc.{ProcElem, Obj}
import de.sciss.lucre.stm.Source
import de.sciss.lucre.synth.Sys

object VisualProc {
  val COLUMN_DATA = "nuages.data"
}
final class VisualProc[S <: Sys[S]](var name: String, var par: Map[String, Double],
                                    val staleCursor: S#Acc, val procH: Source[S#Tx, Obj.T[S, ProcElem]])
