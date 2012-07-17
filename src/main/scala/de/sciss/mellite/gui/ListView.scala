package de.sciss.mellite
package gui

import swing.Component
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.lucre.expr.LinkedList
import impl.{ListViewImpl => Impl}

object ListView {
   def apply[ S <: Sys[ S ], Elem ]( list: LinkedList[ S, Elem, _ ], show: Elem => String )
                                   ( implicit tx: S#Tx ) : ListView[ S ] = Impl( list, show )
}
trait ListView[ S <: Sys[ S ]] extends Disposable[ S#Tx ] {
   def component: Component
}
