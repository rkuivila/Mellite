package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Cursor, Sys}
import de.sciss.synth.proc.Proc

object ProcEditorFrameImpl {
   def apply[ S <: Sys[ S ]]( proc: Proc[ S ])( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcEditorFrame[ S ] = sys.error( "TODO" )
}
