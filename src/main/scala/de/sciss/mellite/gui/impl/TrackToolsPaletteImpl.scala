package de.sciss.mellite
package gui
package impl

import scala.swing.{ToggleButton, Action, Orientation, BoxPanel}
import javax.swing.{ImageIcon, ButtonGroup}
import de.sciss.synth.proc.Sys

final class TrackToolsPaletteImpl[S <: Sys[S]](tools: TrackTools[S]) extends BoxPanel(Orientation.Horizontal) {
  private def getIcon(name: String) = new ImageIcon(Mellite.getClass.getResource(s"icon-$name.png"))
  private val group = new ButtonGroup()

  private val elements = Vector("text", "openhand", "hresize", "vresize", "fade", "hresize", "mute", "audition")

  elements.zipWithIndex.foreach { case (iconName, idx) =>
    val b = new ToggleButton()
    b.action = new Action(null) {
      icon = getIcon(iconName)
      def apply() {
        println(iconName)
      }
    }
    b.focusable = false
    val j = b.peer
    j.putClientProperty("JButton.buttonType", "segmentedCapsule")
    val pos = if (idx == 0) "first" else if (idx == elements.size - 1) "last" else "middle"
    j.putClientProperty("JButton.segmentPosition", pos)
    group.add(j)
    contents += b
  }
}