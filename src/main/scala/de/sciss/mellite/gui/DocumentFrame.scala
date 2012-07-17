package de.sciss.mellite
package gui

import swing.Frame
import impl.{DocumentFrameImpl => Impl}
import de.sciss.lucre.stm.Sys

object DocumentFrame {
   def apply[ S <: Sys[ S ]]( doc: Document[ S ])( implicit tx: S#Tx ) : DocumentFrame[ S ] = Impl( doc )
}
trait DocumentFrame[ S <: Sys[ S ]] {
   def component : Frame
   def document : Document[ S ]
}
