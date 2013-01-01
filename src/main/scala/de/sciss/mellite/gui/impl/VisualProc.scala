package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.{Sys, Proc}
import de.sciss.lucre.stm.Source

object VisualProc {
   val COLUMN_DATA   = "nuages.data"
}
final class VisualProc[ S <: Sys[ S ]]( var name: String, var par: Map[ String, Double ],
                                        val staleCursor: S#Acc, val procH: Source[ S#Tx, Proc[ S ]])
