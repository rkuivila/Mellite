/*
 *  LogFrame.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import de.sciss.scalainterpreter.LogPane
import impl.component.{LogFrameImpl => Impl}
import de.sciss.desktop.Window

object LogFrame {
  val horizontalPlacement   = 1.0f
  val verticalPlacement     = 1.0f
  val placementPadding      = 20

  lazy val instance: LogFrame  = new Impl
}
abstract class LogFrame extends Window {
  def log: LogPane
}