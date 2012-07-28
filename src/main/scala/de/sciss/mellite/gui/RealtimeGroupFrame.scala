package de.sciss.mellite
package gui

import swing.Frame
import de.sciss.lucre.stm.Sys
import impl.{RealtimeGroupFrameImpl => Impl}

object RealtimeGroupFrame {
   def apply[ S <: Sys[ S ]]( group: Document.Group[ S ])( implicit tx: S#Tx ) : RealtimeGroupFrame[ S ] = Impl( group )
}
trait RealtimeGroupFrame[ S <: Sys[ S ]] {
   def component : Frame
   def group : Document.Group[ S ]
}
