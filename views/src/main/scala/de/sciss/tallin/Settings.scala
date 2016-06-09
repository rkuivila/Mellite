package de.sciss.tallin

import de.sciss.file._
import de.sciss.nuages.{NamedBusConfig, ScissProcs, Nuages}

object Settings {
  def apply(nConfig: Nuages.ConfigBuilder, sConfig: ScissProcs.ConfigBuilder): Unit = {
    nConfig.masterChannels    = Some(0 until 2)
    nConfig.soloChannels      = None // Some(8 to 9)
    sConfig.generatorChannels = 2 // 4 // 0
    sConfig.micInputs         = Vector(
      // NamedBusConfig("m-at" , 0, 2),
      NamedBusConfig("m-dpa" , 0, 2)
      // NamedBusConfig("m-hole",  0, 1),
      // NamedBusConfig("m-keys",  1, 1)
    )
    sConfig.lineInputs      = Vector(
      NamedBusConfig("pirro", 4, 2),
      NamedBusConfig("beat" , 6, 1)
    )
    sConfig.lineOutputs     = Vector(
      // NamedBusConfig("sum", 6, 2)
    )
    // sConfig.audioFilesFolder  = Some(userHome / "IEM" / "Impuls2015" / "tapes")
    sConfig.audioFilesFolder  = None // Some(userHome / "Music" / "tapes")
  }
}
