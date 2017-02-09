package de.sciss.mellite

import java.io.File

import de.sciss.filecache.Limit
import de.sciss.fscape.lucre.{FScape, Cache => FScCache}
import de.sciss.mellite.gui.impl.FScapeObjView
import de.sciss.nuages.Wolkenpumpe
import de.sciss.synth.proc.{GenView, SoundProcesses}

trait Init {
  def initTypes(): Unit = {
    SoundProcesses.init()
    Wolkenpumpe   .init()
    FScape        .init()
    FScapeObjView .init()

    val cacheDir  = new File(new File(sys.props("user.home"), "mellite"), "cache")
    cacheDir.mkdirs()
    val cacheLim  = Limit(count = 8192, space = 2L << 10 << 100)  // 2 GB; XXX TODO --- through user preferences
    FScCache.init(folder = cacheDir, capacity = cacheLim)

    val fscapeF = FScape.genViewFactory()
    GenView.tryAddFactory(fscapeF)
  }
}