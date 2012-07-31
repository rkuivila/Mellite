package de.sciss.mellite
package gui

import de.sciss.lucre.stm.{Cursor, Sys}
import impl.{TransportPanelImpl => Impl}
import swing.Component

object TransportPanel {
   def apply[ S <: Sys[ S ]]( transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : TransportPanel[ S ] = Impl( transport )
}
trait TransportPanel[ S <: Sys[ S ]] {
   def component: Component
   def transport( implicit tx: S#Tx ) : Document.Transport[ S ]
}
