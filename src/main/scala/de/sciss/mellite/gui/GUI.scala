/*
 *  GUI.scala
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

package de.sciss
package mellite
package gui

import scala.swing.Swing._
import scala.swing.{Action, Color, Button, AbstractButton, Swing, Dialog, Component, TextField, Label, Alignment}
import java.awt.{Rectangle, GraphicsEnvironment}
import javax.swing.{Icon, Timer}
import de.sciss.swingplus.GroupPanel
import de.sciss.icons.raphael
import java.awt.geom.Path2D

// XXX TODO: this stuff should go somewhere for re-use.
object GUI {
  def centerOnScreen(w: desktop.Window): Unit = placeWindow(w, 0.5f, 0.5f, 0)

  def delay(millis: Int)(block: => Unit): Unit = {
    val timer = new Timer(millis, Swing.ActionListener(_ => block))
    timer.setRepeats(false)
    timer.start()
  }

  def fixWidth(c: Component, width: Int = -1): Unit = {
    val w         = if (width < 0) c.preferredSize.width else width
    val min       = c.minimumSize
    val max       = c.maximumSize
    min.width     = w
    max.width     = w
    c.minimumSize = min
    c.maximumSize = max
  }

  def round(b: AbstractButton*): Unit =
    b.foreach(_.peer.putClientProperty("JButton.buttonType", "roundRect"))

  def findWindow(c: Component): Option[desktop.Window] =
    None  // XXX TODO - we should place a client property in Desktop

  def maximumWindowBounds: Rectangle = {
    val ge  = GraphicsEnvironment.getLocalGraphicsEnvironment
    ge.getMaximumWindowBounds
  }

  def placeWindow(w: desktop.Window, horizontal: Float, vertical: Float, padding: Int): Unit = {
    val bs  = maximumWindowBounds
    val b   = w.size
    val x   = (horizontal * (bs.width  - padding * 2 - b.width )).toInt + bs.x + padding
    val y   = (vertical   * (bs.height - padding * 2 - b.height)).toInt + bs.y + padding
    w.location = (x, y)
  }

  def keyValueDialog(value: Component, title: String = "New Entry", defaultName: String = "Name",
                     window: Option[desktop.Window] = None): Option[String] = {
    val ggName  = new TextField(10)
    ggName.text = defaultName

    val box = new GroupPanel {
      val lbName  = new Label( "Name:", EmptyIcon, Alignment.Right)
      val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
      horizontal  = Seq(Par(Trailing)(lbName, lbValue), Par          (ggName , value))
      vertical    = Seq(Par(Baseline)(lbName, ggName ), Par(Baseline)(lbValue, value))
    }

    val pane = desktop.OptionPane.confirmation(box, optionType = Dialog.Options.OkCancel,
      messageType = Dialog.Message.Question, focus = Some(value))
    pane.title  = title
    val res = pane.show(window)

    if (res == Dialog.Result.Ok) {
      val name    = ggName.text
      Some(name)
    } else {
      None
    }
  }

  private def iconNormal(fun: Path2D => Unit): Icon =
    raphael.Icon(extent = 20, fill = raphael.TexturePaint(24), shadow = raphael.WhiteShadow)(fun)

  private def iconDisabled(fun: Path2D => Unit): Icon =
    raphael.Icon(extent = 20, fill = new Color(0, 0, 0, 0x7F), shadow = raphael.WhiteShadow)(fun)

  def toolButton(action: Action, iconFun: Path2D => Unit, tooltip: String = ""): Button = {
    val res           = new Button(action)
    res.icon          = iconNormal  (iconFun)
    res.disabledIcon  = iconDisabled(iconFun)
    res.peer.putClientProperty("JButton.buttonType", "textured")
    if (!tooltip.isEmpty) res.tooltip = tooltip
    res
  }
}