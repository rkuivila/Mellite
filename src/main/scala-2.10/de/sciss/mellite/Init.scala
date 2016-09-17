package de.sciss.mellite

import de.sciss.nuages.Wolkenpumpe
import de.sciss.synth.proc.SoundProcesses

trait Init {
  def initTypes(): Unit = {
    SoundProcesses.init()
    Wolkenpumpe   .init()
    // FScape        .init()
  }
}