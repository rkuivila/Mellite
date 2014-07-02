/*
 *  LogFrame.scala
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

package de.sciss.mellite
package gui

import de.sciss.scalainterpreter.Style
import de.sciss.desktop.{LogPane, WindowHandler, Window}
import de.sciss.desktop.impl.LogWindowImpl
import java.awt.Font

object LogFrame {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20

  lazy val instance: LogFrame  = new LogWindowImpl with LogFrame { frame =>
    def handler: WindowHandler = Application.windowHandler

    log.background  = Style.BlueForest.background
    log.foreground  = Style.BlueForest.foreground
    log.font        = new Font(Font.MONOSPACED, Font.PLAIN, 10)

    pack()  // after changing font!
    GUI.placeWindow(frame, horizontal = horizontalPlacement, vertical = verticalPlacement, padding = placementPadding)
  }
}
trait LogFrame extends Window {
  def log: LogPane
}