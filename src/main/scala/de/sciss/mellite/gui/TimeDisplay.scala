package de.sciss.mellite
package gui

import scala.swing.Component
import impl.{TimeDisplayImpl => Impl}

object TimeDisplay {
  def apply(model: TimelineModel): TimeDisplay = new Impl(model)
}
trait TimeDisplay {
  def component: Component
}