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

import de.sciss.nuages.Wolkenpumpe
import de.sciss.synth.proc.SoundProcesses

trait Init {
  def initTypes(): Unit = {
    SoundProcesses.init()
    Wolkenpumpe   .init()
    // FScape        .init()
  }
}