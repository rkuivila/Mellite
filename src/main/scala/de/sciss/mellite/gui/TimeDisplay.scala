package de.sciss.mellite
package gui

import scala.swing.Component
import impl.{TimeDisplayImpl => Impl}
import de.sciss.audiowidgets.TimelineModel

object TimeDisplay {
  def apply(model: TimelineModel): TimeDisplay = new Impl(model)
}
trait TimeDisplay {
  def component: Component
}