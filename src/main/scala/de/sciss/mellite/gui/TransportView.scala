/*
 *  TransportView.scala
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