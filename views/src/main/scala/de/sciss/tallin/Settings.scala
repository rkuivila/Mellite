package de.sciss.tallin

import de.sciss.file._
import de.sciss.nuages.{NamedBusConfig, ScissProcs, Nuages}

object Settings {
  def apply(nConfig: Nuages.ConfigBuilder, sConfig: ScissProcs.ConfigBuilder): Unit = {
    nConfig.masterChannels    = Some(0 to 5)
    nConfig.soloChannels      = Some(8 to 9)
    sConfig.generatorChannels = 0
    sConfig.micInputs         = Vector(
      // NamedBusConfig("m-at" , 0, 2),
      NamedBusConfig("m-dpa" , 0, 2)
      // NamedBusConfig("m-hole",  0, 1),
      // NamedBusConfig("m-keys",  1, 1)
    )
    sConfig.lineInputs      = Vector(
      NamedBusConfig("pirro", 4, 1),
      NamedBusConfig("beat" , 5, 1)
    )
    sConfig.lineOutputs     = Vector(
      NamedBusConfig("sum", 6, 2)
    )
    // sConfig.audioFilesFolder  = Some(userHome / "IEM" / "Impuls2015" / "tapes")
    sConfig.audioFilesFolder  = Some(userHome / "Music" / "tapes")
  }
}
