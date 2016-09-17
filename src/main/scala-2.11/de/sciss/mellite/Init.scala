package de.sciss.mellite

import de.sciss.fscape.lucre.FScape
import de.sciss.mellite.gui.impl.FScapeObjView
import de.sciss.nuages.Wolkenpumpe
import de.sciss.synth.proc.SoundProcesses

trait Init {
  def initTypes(): Unit = {
    SoundProcesses.init()
    Wolkenpumpe   .init()
    FScape        .init()
    FScapeObjView .init()
  }
}