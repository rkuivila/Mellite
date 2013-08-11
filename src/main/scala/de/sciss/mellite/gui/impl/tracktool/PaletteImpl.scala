/*
 *  PaletteImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl
package tracktool

import scala.swing.{ToggleButton, Action, Orientation, BoxPanel}
import javax.swing.{KeyStroke, ButtonGroup}
import collection.immutable.{IndexedSeq => Vec}
import java.awt.event.KeyEvent
import de.sciss.desktop.FocusType
import de.sciss.synth.proc.Sys
import de.sciss.desktop

final class PaletteImpl[S <: Sys[S]](control: TrackTools[S], tools: Vec[TrackTool[S, _]])
  extends BoxPanel(Orientation.Horizontal) {

  private val group = new ButtonGroup()

  // private val elements = Vector("text", "openhand", "hresize", "vresize", "fade", "hresize", "mute", "audition")

  private val sz = tools.size

  tools.zipWithIndex.foreach { case (tool, idx) =>
    val b     = new ToggleButton()
    val name  = tool.name
    b.action  = new Action(null) {
      icon = tool.icon
      def apply(): Unit = control.currentTool = tool // println(name)
    }
    b.focusable = false
    val j = b.peer
    j.putClientProperty("JButton.buttonType", "segmentedCapsule")
    val pos = if (sz == 1) "only" else if (idx == 0) "first" else if (idx == sz - 1) "last" else "middle"
    j.putClientProperty("JButton.segmentPosition", pos)
    group.add(j)
    if (idx == 0) {
      b.selected = true
    }
    import desktop.Implicits._
    b.addAction(key = s"tracktool-$name", focus = FocusType.Window, action = new Action(name) {
      accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_1 + idx, 0))
      def apply(): Unit = b.doClick()
    })
    contents += b
  }
}