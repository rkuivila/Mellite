package de.sciss.mellite
package gui

import scala.swing.Component
import de.sciss.audiowidgets.TimelineModel
import impl.component.{TimeDisplayImpl => Impl}

object TimeDisplay {
  def apply(model: TimelineModel): TimeDisplay = new Impl(model)
}
trait TimeDisplay {
  def component: Component
}