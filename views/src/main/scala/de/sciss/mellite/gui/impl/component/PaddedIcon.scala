package de.sciss.mellite.gui.impl.component

import javax.swing.Icon
import scala.swing.Insets
import java.awt.{Graphics, Component}

class PaddedIcon(inner: Icon, insets: Insets) extends Icon {
  def getIconWidth : Int = inner.getIconWidth  + insets.left + insets.right
  def getIconHeight: Int = inner.getIconHeight + insets.top  + insets.bottom

  def paintIcon(c: Component, g: Graphics, x: Int, y: Int): Unit =
    inner.paintIcon(c, g, x + insets.left, y + insets.top)
}
