package de.sciss.mellite
package gui

import de.sciss.scalainterpreter.LogPane
import impl.{LogFrameImpl => Impl}
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