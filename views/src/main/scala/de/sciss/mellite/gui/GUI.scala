/*
 *  GUI.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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
import java.awt.geom.{AffineTransform, Area, Path2D}
import java.awt.{BasicStroke, Graphics, Graphics2D, RenderingHints, Shape}
import javax.swing.{Icon, JComponent, KeyStroke, SwingUtilities}

import de.sciss.audiowidgets.Transport
import de.sciss.desktop.{KeyStrokes, OptionPane}
import de.sciss.icons.raphael
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.{defer, requireEDT}
import de.sciss.swingplus.{DoClickAction, GroupPanel}
import de.sciss.synth.proc.SoundProcesses
import org.scalautils.TypeCheckedTripleEquals

import scala.concurrent.Future
import scala.swing.Swing._
import scala.swing.event.Key
import scala.swing.{AbstractButton, Action, Alignment, Button, Component, Dialog, Label, TextField}

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
    import TypeCheckedTripleEquals._
    if (res === Dialog.Result.Ok) {
      val name    = ggName.text
      Some(name)
    } else {
      None
    }
  }

  def iconNormal  (fun: Path2D => Unit): Icon = raphael.TexturedIcon        (20)(fun)
  def iconDisabled(fun: Path2D => Unit): Icon = raphael.TexturedDisabledIcon(20)(fun)

  private val sharpStrk       = new BasicStroke(1f)
  private val sharpShadowYOff = 1f

  private final class SharpIcon(extent: Int, scheme: Transport.ColorScheme, outShape: Shape,
                                inShape: Shape) extends Icon {
    def getIconWidth : Int = extent
    def getIconHeight: Int = extent

    def paintIcon(c: java.awt.Component, g: Graphics, x: Int, y: Int): Unit = {
      val g2 = g.asInstanceOf[Graphics2D]
      g2.setRenderingHint(RenderingHints.KEY_ANTIALIASING  , RenderingHints.VALUE_ANTIALIAS_ON)
      g2.setRenderingHint(RenderingHints.KEY_STROKE_CONTROL, RenderingHints.VALUE_STROKE_PURE )
      val atOrig = g2.getTransform
      g2.translate(x + 1, y + 1 + sharpShadowYOff)
      g2.setPaint(scheme.shadowPaint)
      g2.fill(outShape)
      g2.translate(0, -sharpShadowYOff)
      g2.setPaint(scheme.outlinePaint)
      g2.fill(outShape)
      g2.setPaint(scheme.fillPaint(extent.toFloat / 20))
      g2.fill(inShape)
      g2.setTransform(atOrig)
    }
  }

  /** No texture, for more finely drawn icons. */
  def sharpIcon(fun: Path2D => Unit): Icon = {
    val in0 = new Path2D.Float()
    fun(in0)
    val scale = 19.0 / 32
    val in  = AffineTransform.getScaleInstance(scale, scale).createTransformedShape(in0)
    val out = new Area(sharpStrk.createStrokedShape(in))
    out.add(new Area(in))
    val scheme = if (Mellite.isDarkSkin) Transport.LightScheme else Transport.DarkScheme
    new SharpIcon(extent = 20, scheme = scheme, out, in)
  }

  def toolButton(action: Action, iconFun: Path2D => Unit, tooltip: String = ""): Button = {
    val res           = new Button(action)
    res.peer.putClientProperty("styleId", "icon-space")
    res.icon          = iconNormal  (iconFun)
    res.disabledIcon  = iconDisabled(iconFun)
    // res.peer.putClientProperty("JButton.buttonType", "textured")
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