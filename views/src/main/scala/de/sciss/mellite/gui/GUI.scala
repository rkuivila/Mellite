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
import scala.swing.{Action, Color, Button, AbstractButton, Dialog, Component, TextField, Label, Alignment}
import javax.swing.{JComponent, Icon}
import de.sciss.swingplus.GroupPanel
import de.sciss.icons.raphael
import java.awt.geom.Path2D
import de.sciss.mellite.gui.impl.WindowImpl
import scala.annotation.tailrec

// XXX TODO: this stuff should go somewhere for re-use.
object GUI {
  private def wordWrap(s: String, margin: Int = 80): String = {
    if (s == null) return "" // fuck java
    val sz = s.length
    if (sz <= margin) return s
    var i = 0
    val sb = new StringBuilder
    while (i < sz) {
      val j = s.lastIndexOf(" ", i + margin)
      val found = j > i
      val k = if (found) j else i + margin
      sb.append(s.substring(i, math.min(sz, k)))
      i = if (found) k + 1 else k
      if (i < sz) sb.append('\n')
    }
    sb.toString()
  }

  def formatException(e: Throwable): String = {
    e.getClass.toString + " :\n" + wordWrap(e.getMessage) + "\n" +
      e.getStackTrace.take(10).map("   at " + _).mkString("\n")
  }

  def round(b: AbstractButton*): Unit =
    b.foreach(_.peer.putClientProperty("JButton.buttonType", "roundRect"))

  def findWindow(c: Component): Option[desktop.Window] = {
    @tailrec def loop(p: JComponent): Option[desktop.Window] =
      p.getClientProperty(WindowImpl.WindowKey) match {
        case f: desktop.Window => Some(f)
        case _ => c.peer.getParent match {
          case pp: JComponent => loop(pp)
          case _ => None
        }
      }

    loop(c.peer)
  }

  def keyValueDialog(value: Component, title: String = "New Entry", defaultName: String = "Name",
                     window: Option[desktop.Window] = None): Option[String] = {
    val ggName  = new TextField(10)
    ggName.text = defaultName
    val lbName  = new Label( "Name:", EmptyIcon, Alignment.Right)
    val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)

    val box = new GroupPanel {
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

  def iconNormal(fun: Path2D => Unit): Icon =
    raphael.Icon(extent = 20, fill = raphael.TexturePaint(24), shadow = raphael.WhiteShadow)(fun)

  def iconDisabled(fun: Path2D => Unit): Icon =
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