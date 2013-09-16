/*
 *  TransportView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite
package gui

import synth.proc
import proc.{AuralSystem, AuralPresentation, ProcGroup, ProcTransport}
import lucre.stm
import impl.{TransportViewImpl => Impl}
import stm.Disposable
import scala.swing.Component
import de.sciss.audiowidgets.TimelineModel
import de.sciss.lucre.synth.Sys

object TransportView {
  def apply[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S], sampleRate: Double, timelineModel: TimelineModel)
                                         (implicit tx: S#Tx, cursor: stm.Cursor[S], 
                                          bridge: S#Tx => I#Tx, aural: AuralSystem): TransportView[S] =
    Impl[S, I](group, sampleRate, timelineModel)
}
trait TransportView[S <: Sys[S]] extends Disposable[S#Tx] {
  def transport: ProcTransport[S]
  def auralPresentation: AuralPresentation[S]

  def component: Component
}