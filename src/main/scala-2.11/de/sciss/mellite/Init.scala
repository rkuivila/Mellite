/*
 *  Init.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import java.io.File

import de.sciss.filecache.Limit
import de.sciss.fscape.lucre.{FScape, Cache => FScCache}
import de.sciss.fscape.stream.Control
import de.sciss.mellite.gui.impl.{FScapeObjView, FScapeOutputObjView}
import de.sciss.nuages.Wolkenpumpe
import de.sciss.synth.proc.{GenView, SoundProcesses}

trait Init {
  def initTypes(): Unit = {
    SoundProcesses      .init()
    Wolkenpumpe         .init()
    FScape              .init()
    FScapeObjView       .init()
    FScapeOutputObjView .init()

    val cacheDir  = new File(new File(sys.props("user.home"), "mellite"), "cache")
    cacheDir.mkdirs()
    val cacheLim  = Limit(count = 8192, space = 2L << 10 << 100)  // 2 GB; XXX TODO --- through user preferences
    FScCache.init(folder = cacheDir, capacity = cacheLim)

    val ctlConf = Control.Config()
    ctlConf.terminateActors = false
    val fscapeF = FScape.genViewFactory(ctlConf)
    GenView.tryAddFactory(fscapeF)
  }
}