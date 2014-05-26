/*
 *  TimeDisplay.scala
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

import scala.swing.Component
import de.sciss.audiowidgets.TimelineModel
import impl.component.{TimeDisplayImpl => Impl}

object TimeDisplay {
  def apply(model: TimelineModel, hasMillis: Boolean): TimeDisplay = new Impl(model, hasMillis = hasMillis)
}
trait TimeDisplay {
  def component: Component
}