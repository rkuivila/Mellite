/*
 *  PaletteImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl
package tracktool

import java.awt.event.KeyEvent
import javax.swing.{ButtonGroup, KeyStroke}

import de.sciss.desktop
import de.sciss.desktop.FocusType
import de.sciss.lucre.synth.Sys

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.swing.{Action, BoxPanel, Orientation, ToggleButton}

// note: ToolBar looks crap in WebLookAndFeel -- has ugly borders
final class PaletteImpl[S <: Sys[S]](control: TrackTools[S], tools: Vec[TrackTool[S, _]])
  extends /* ToolBar */ BoxPanel(Orientation.Horizontal) {

  private val group = new ButtonGroup()

  //  floatable     = false
  //  borderPainted = false
  //  border        = null

  // private val elements = Vector("text", "openhand", "hresize", "vresize", "fade", "hresize", "mute", "audition")

  // private val sz = tools.size

  tools.zipWithIndex.foreach { case (tool, idx) =>
    val b     = new ToggleButton()
    b.peer.putClientProperty("styleId", "icon-space")
    val name  = tool.name
    b.action  = new Action(null) {
      icon    = tool.icon
      toolTip = tool.name
      def apply(): Unit = control.currentTool = tool // println(name)
    }
    b.focusable = false
    val j = b.peer
    //    j.putClientProperty("JButton.buttonType", "segmentedCapsule")
    //    val pos = if (sz == 1) "only" else if (idx == 0) "first" else if (idx == sz - 1) "last" else "middle"
    //    j.putClientProperty("JButton.segmentPosition", pos)
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