/*
 *  TransportPanel.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import de.sciss.lucre.stm.Cursor
import impl.{TransportPanelImpl => Impl}
import swing.Component
import de.sciss.lucre.synth.Sys

object TransportPanel {
   def apply[ S <: Sys[ S ]]( transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : TransportPanel[ S ] = Impl( transport )
}
trait TransportPanel[ S <: Sys[ S ]] {
   def component: Component
   def transport /* ( implicit tx: S#Tx ) */ : Document.Transport[ S ]
}
