package de.sciss.mellite
package gui

import de.sciss.lucre.stm.{Cursor, Sys}
import swing.Component
import de.sciss.synth.proc.Proc
import impl.{ProcEditorFrameImpl => Impl}

object ProcEditorFrame {
   def apply[ S <: Sys[ S ]]( proc: Proc[ S ])( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcEditorFrame[ S ] =
      Impl( proc )
}
trait ProcEditorFrame[ S <: Sys[ S ]] {
   def component: Component
   def proc( implicit tx: S#Tx ) : Proc[ S ]
}
