package de.sciss.impuls2015

import de.sciss.nuages.{NamedBusConfig, ScissProcs, Nuages}

object Settings {
  def apply(nConfig: Nuages.ConfigBuilder, sConfig: ScissProcs.ConfigBuilder): Unit = {
    nConfig.masterChannels    = Some(0 to 1)
    nConfig.soloChannels      = Some(6 to 7)
    sConfig.generatorChannels = 0
    sConfig.micInputs         = Vector(
      // NamedBusConfig("m-at" , 0, 2),
      NamedBusConfig("m-dpa" , 10, 1),
      NamedBusConfig("m-hole",  0, 1),
      NamedBusConfig("m-keys",  1, 1)
    )
    sConfig.lineInputs      = Vector(
      // NamedBusConfig("beat" , 3, 1),
      // NamedBusConfig("pirro", 4, 1)
    )
    sConfig.lineOutputs     = Vector(
      NamedBusConfig("sum", 4, 2)
    )
  }
}
