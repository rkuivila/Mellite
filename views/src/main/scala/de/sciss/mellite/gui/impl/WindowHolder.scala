/*
 *  WindowHolder.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite.gui.impl

import de.sciss.desktop.Window
import de.sciss.lucre.swing.requireEDT

trait WindowHolder[W <: Window] {
  private var _window: W = _

  def window: W = {
    requireEDT()
    if (_window == null) throw new IllegalStateException("Called component before GUI was initialized")
    _window
  }

  final protected def window_=(value: W): Unit = {
    requireEDT()
    if (_window != null) throw new IllegalStateException("Window has already been set")
    _window = value
  }
}
