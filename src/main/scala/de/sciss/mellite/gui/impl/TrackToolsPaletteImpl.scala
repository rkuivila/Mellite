package de.sciss
package mellite
package gui
package impl

import scala.swing.{ToggleButton, Action, Orientation, BoxPanel}
import javax.swing.{KeyStroke, ImageIcon, ButtonGroup}
import collection.immutable.{IndexedSeq => IIdxSeq}
import synth.proc.Sys
import java.awt.event.KeyEvent
import de.sciss.desktop.FocusType

final class TrackToolsPaletteImpl[S <: Sys[S]](control: TrackTools[S], tools: IIdxSeq[TrackTool[S, _]])
  extends BoxPanel(Orientation.Horizontal) {

  private val group = new ButtonGroup()

  // private val elements = Vector("text", "openhand", "hresize", "vresize", "fade", "hresize", "mute", "audition")

  private val sz = tools.size

  tools.zipWithIndex.foreach { case (tool, idx) =>
    val b     = new ToggleButton()
    val name  = tool.name
    b.action  = new Action(null) {
      icon = tool.icon
      def apply() {
        control.currentTool = tool // println(name)
      }
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
      def apply() {
        // control.currentTool = tool
        b.doClick()
      }
    })
    contents += b
  }
}