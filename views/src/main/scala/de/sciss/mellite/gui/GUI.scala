/*
 *  GUI.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
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

import java.awt.event.{ActionEvent, ActionListener}
import java.awt.geom.Path2D
import javax.swing.{SwingUtilities, Icon, JComponent, KeyStroke}

import de.sciss.desktop.{OptionPane, KeyStrokes}
import de.sciss.icons.raphael
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm
import de.sciss.lucre.swing.{defer, requireEDT}
import de.sciss.swingplus.{DoClickAction, GroupPanel}
import de.sciss.synth.proc.SoundProcesses

import scala.concurrent.Future
import scala.swing.Swing._
import scala.swing.event.Key
import scala.swing.{AbstractButton, Action, Alignment, Button, Color, Component, Dialog, Label, TextField}

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

  /** Adds a global key-triggered click action to a button. The key
    * is active if the action appears in the focused window. The added action
    * simulates a button click.
    */
  def addGlobalKey(b: AbstractButton, keyStroke: KeyStroke): Unit = {
    val click = DoClickAction(b)
    b.peer.getActionMap.put("click", click.peer)
    b.peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(keyStroke, "click")
  }

  def viewButton(action: Action, tooltip: String = ""): Button = {
    val res = toolButton(action, raphael.Shapes.View, tooltip)
    addGlobalKey(res, KeyStrokes.menu1 + Key.Enter)
    res
  }

  def attrButton(action: Action, tooltip: String = ""): Button = {
    val res = toolButton(action, raphael.Shapes.Wrench, tooltip)
    addGlobalKey(res, KeyStrokes.menu1 + Key.Semicolon)
    res
  }

  def addButton(action: Action, tooltip: String = ""): Button = {
    val res = toolButton(action, raphael.Shapes.Plus, tooltip)
    addGlobalKey(res, KeyStrokes.menu1 + Key.N)
    res
  }

  def removeButton(action: Action, tooltip: String = ""): Button = {
    val res = toolButton(action, raphael.Shapes.Minus, tooltip)
    addGlobalKey(res, KeyStrokes.menu1 + Key.BackSpace)
    res
  }

  def duplicateButton(action: Action, tooltip: String = ""): Button = {
    val res = toolButton(action, raphael.Shapes.SplitArrows, tooltip)
    addGlobalKey(res, KeyStrokes.menu1 + Key.D)
    res
  }

  def atomic[S <: Sys[S], A](title: String, message: String, window: Option[desktop.Window] = None,
                             timeout: Int = 1000)(fun: S#Tx => A)
                            (implicit cursor: stm.Cursor[S]): Future[A] = {
    requireEDT()
    val res = SoundProcesses.atomic[S, A](fun)
    var opt: OptionPane[Unit] = null
    val t = new javax.swing.Timer(timeout, new ActionListener {
      def actionPerformed(e: ActionEvent): Unit = {
        if (!res.isCompleted) {
          opt = OptionPane.message(message = s"$message…")
          opt.show(window, title)
        }
      }
    })
    t.setRepeats(false)
    t.start()
    res.onComplete { _ =>
      t.stop()
      defer {
        if (opt != null) {
          val w = SwingUtilities.getWindowAncestor(opt.peer)
          if (w != null) w.dispose()
        }
      }
    }
    res
  }
}