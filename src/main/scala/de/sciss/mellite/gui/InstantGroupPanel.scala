/*
 *  VisualInstantPresentation.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui

import de.sciss.lucre.stm.Cursor
import de.sciss.synth.proc
import proc.ProcTransport
import impl.realtime.{PanelImpl => Impl}
import swing.Component
import de.sciss.lucre.synth.Sys

object InstantGroupPanel {
  def apply[S <: Sys[S]](transport: ProcTransport[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupPanel[S] = Impl(transport)
}

trait InstantGroupPanel[S <: Sys[S]] {
  def component: Component
}
