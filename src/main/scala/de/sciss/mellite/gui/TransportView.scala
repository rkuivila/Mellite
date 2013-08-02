package de.sciss
package mellite
package gui

import synth.proc
import proc.{AuralSystem, AuralPresentation, Sys, ProcGroup, ProcTransport}
import lucre.stm
import impl.{TransportViewImpl => Impl}
import stm.Disposable
import scala.swing.Component
import de.sciss.audiowidgets.TimelineModel

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