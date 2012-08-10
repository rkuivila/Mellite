package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Proc
import de.sciss.lucre.stm.{Cursor, Sys}

object VisualProc {
   val COLUMN_DATA   = "nuages.data"
}
final class VisualProc[ S <: Sys[ S ]]( var name: String, var par: Map[ String, Double ],
                                        val staleCursor: S#Acc, val proc: Proc[ S ])
