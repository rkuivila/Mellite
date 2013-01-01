package de.sciss.mellite
package gui

import swing.Frame
import de.sciss.lucre.stm.Cursor
import impl.{InstantGroupFrameImpl => Impl}
import de.sciss.synth.proc.Sys

object InstantGroupFrame {
   def apply[ S <: Sys[ S ]]( group: Document.Group[ S ], transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : InstantGroupFrame[ S ] =
      Impl( group, transport )
}
trait InstantGroupFrame[ S <: Sys[ S ]] {
   def component : Frame
   def group( implicit tx: S#Tx ) : Document.Group[ S ]
   def transport /* ( implicit tx: S#Tx ) */ : Document.Transport[ S ]
}
