/*
 *  SpinningProgressBar.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui.impl.component

import de.sciss.swingplus.OverlayPanel
import scala.swing.{Swing, ProgressBar}
import Swing._
import de.sciss.lucre.swing.defer

class SpinningProgressBar extends OverlayPanel {
  @volatile private var _spin = false

  /** This method is thread safe. */
  def spinning: Boolean = _spin
  def spinning_=(value: Boolean): Unit = {
    _spin = value
    defer {
      ggBusy.visible = _spin
    }
  }

  private val ggBusy: ProgressBar = new ProgressBar {
    visible       = false
    indeterminate = true
    preferredSize = (24, 24)
    peer.putClientProperty("JProgressBar.style", "circular")
  }

  contents += RigidBox((24, 24))
  contents += ggBusy
}
