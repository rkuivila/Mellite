/*
 *  MainFrame.scala
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

package de.sciss
package mellite
package gui

import desktop.impl.WindowImpl
import desktop.{Window, WindowHandler}
import scala.swing.{FlowPanel, ToggleButton, Action, Button, Label, Slider, Component, Orientation, BoxPanel, Swing}
import de.sciss.synth.proc.{TxnPeek, Server, AuralSystem}
import de.sciss.synth.swing.{AudioBusMeter, ServerStatusPanel}
import de.sciss.synth.{addToTail, SynthDef, addToHead, AudioBus}
import Swing._
import javax.swing.UIManager
import java.awt.{Color, Font}
import java.util.Locale
import scala.swing.event.ValueChanged
import collection.breakOut
import javax.swing.border.Border

final class MainFrame extends WindowImpl { me =>
  import Mellite.auralSystem

  def handler: WindowHandler = Mellite.windowHandler

  protected def style: Window.Style = Window.Regular

  private val serverPane = new ServerStatusPanel()
  serverPane.bootAction = Some(boot _)

  private val boxPane = new BoxPanel(Orientation.Vertical)
  boxPane.contents += serverPane

  //  def setServer(s: Option[Server]): Unit serverPane.server = s.map(_.peer)

  component.peer.getRootPane.putClientProperty("Window.style", "small")
  component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
  resizable = false
  contents  = boxPane

  private def boot(): Unit = {
    val config        = Server.Config()
    val audioDevice   = Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice)
    if (audioDevice != Prefs.defaultAudioDevice) config.deviceName = Some(audioDevice)
    config.outputBusChannels = Prefs.audioNumOutputs.getOrElse(Prefs.defaultAudioNumOutputs)
    config.transport  = osc.TCP
    config.pickPort()
    auralSystem.start(config)
  }

  private var meter       = Option.empty[AudioBusMeter]
  private var onlinePane  = Option.empty[Component]

    // val fnt0  = UIManager.getFont("Slider.font", Locale.US)
  private val smallFont   = // if (fnt0 != null)
      // fnt0.deriveFont(math.min(fnt0.getSize2D, 9.5f))
    // else
      new Font("SansSerif", Font.PLAIN, 9)

  private val tinyFont = new Font("SansSerif", Font.PLAIN, 7)

  private def started(s: Server): Unit = {
    // XXX TODO: dirty dirty
    TxnPeek { implicit tx =>
      for(_ <- 1 to 4) s.nextNodeID()
    }
    Swing.onEDT {
      import synth.Ops._

      serverPane.server = Some(s.peer)
      val numOuts = s.peer.config.outputBusChannels
      val outBus  = AudioBus(s.peer, 0, numOuts)
      // val hpBus   = RichBus.audio(s, 2)
      val group   = synth.Group.after(s.peer.defaultGroup)
      val mGroup  = synth.Group.head(group)
      val df = SynthDef("$mellite-master") {
        import synth._
        import ugen._
        val in        = In.ar(0, numOuts)
        val mainAmp   = Lag.ar(K2A.ar("amp".kr(1)))
        val mainIn    = in * mainAmp
        val mainLim   = Limiter.ar(mainIn, level = -0.2.dbamp)
        val lim       = Lag.ar(K2A.ar("limiter".kr(1) * 2 - 1))
        val mainOut   = LinXFade2.ar(mainIn, mainLim, pan = lim)
        val hpBusL    = "hp-bus".kr(0)
        val hpBusR    = hpBusL + 1
        val hpAmp     = Lag.ar(K2A.ar("hp-amp".kr(1)))
        val hpInL     = Mix.tabulate(numOuts + 1 / 2)(i => in \ (i * 2))
        val hpInR     = Mix.tabulate(numOuts     / 2)(i => in \ (i * 2 + 1))
        val hpLimL    = Limiter.ar(hpInL * hpAmp, level = -0.2.dbamp)
        val hpLimR    = Limiter.ar(hpInR * hpAmp, level = -0.2.dbamp)

        val hpActive  = Lag.ar(K2A.ar("hp".kr(0)))
        val out       = (0 until numOuts).map { i =>
          val isL   = hpActive & (hpBusL sig_== i)
          val isR   = hpActive & (hpBusR sig_== i)
          val isHP  = isL | isR
          (mainOut \ i) * (1 - isHP) + hpLimL * isL + hpLimR * isR
        }

        ReplaceOut.ar(0, out)
      }
      val syn = df.play(target = group, addAction = addToTail,
        args = Seq("hp-bus" -> Prefs.headphonesBus.getOrElse(Prefs.defaultHeadphonesBus)))

      val m = AudioBusMeter(AudioBusMeter.Strip(outBus, mGroup, addToHead) :: Nil)
      meter = Some(m)
      val p = new FlowPanel() // new BoxPanel(Orientation.Horizontal)
      p.contents += m.component
      p.contents += HStrut(8)

      def mkAmpFader(ctl: String) = mkFader { db =>
        import synth._
        val amp = if (db == -72) 0f else db.dbamp
        syn.set(ctl -> amp)
      }

      val ggMainVolume  = mkAmpFader("amp")
      val ggHPVolume    = mkAmpFader("hp-amp")

      def mkToggle(label: String, selected: Boolean = false)(fun: Boolean => Unit): ToggleButton = {
        val res = new ToggleButton
        res.action = Action(label) {
          fun(res.selected)
        }
        res.peer.putClientProperty("JComponent.sizeVariant", "mini")
        res.peer.putClientProperty("JButton.buttonType", "square")
        res.selected  = selected
        res.focusable = false
        res
      }

      val ggPost = mkToggle("post") { post =>
        if (post) mGroup.moveToTail(group) else mGroup.moveToHead(group)
      }

      val ggLim = mkToggle("limiter", selected = true) { lim =>
        val on = if (lim) 1f else 0f
        syn.set("limiter" -> on)
      }

      val ggHPActive = mkToggle("active") { active =>
        val on = if (active) 1f else 0f
        syn.set("hp" -> on)
      }

      def mkBorder(label: String): Border = {
        val res = TitledBorder(LineBorder(Color.gray), label)
        res.setTitleFont(smallFont)
        res.setTitleJustification(javax.swing.border.TitledBorder.CENTER)
        res
      }

      val stripMain = new BoxPanel(Orientation.Vertical) {
        contents += ggPost
        contents += ggLim
        contents += ggMainVolume
        border    = mkBorder("Main")
      }

      val stripHP = new BoxPanel(Orientation.Vertical) {
        contents += VStrut(ggPost.preferredSize.height)
        contents += ggHPActive
        contents += ggHPVolume
        border    = mkBorder("Phones")
      }

      p.contents += stripMain
      p.contents += stripHP

      p.contents += HGlue
      onlinePane = Some(p)

      boxPane.contents += p
      // resizable = true
      pack()
    }
  }

  private def mkFader(fun: Int => Unit): Slider = {

    println(s"Font is $smallFont")

    val zeroMark    = "0\u25C0"
    val lbMap: Map[Int, Label] = (-72 to 18 by 12).map { dec =>
      val txt = if (dec == -72) "-\u221E" else if (dec == 0) zeroMark else dec.toString
      val lb  = new Label(txt)
      lb.font = smallFont
      (dec, lb)
    } (breakOut)
    val lbZero = lbMap(0)
    var isZero = true

    val sl    = new Slider {
      orientation       = Orientation.Vertical
      min               = -72
      max               =  18
      value             =   0
      minorTickSpacing  =   3
      majorTickSpacing  =  12
      paintTicks        = true
      paintLabels       = true

      peer.putClientProperty("JComponent.sizeVariant", "small")
      peer.putClientProperty("JSlider.isFilled", true)   // used by Metal-lnf
      labels            = lbMap

      listenTo(this)
      reactions += {
        case ValueChanged(_) =>
          fun(value)
          if (isZero) {
            if (value != 0) {
              isZero = false
              lbZero.text = "0"
              repaint()
            }
          } else {
            if (value == 0) {
              isZero = true
              lbZero.text = zeroMark
              repaint()
            }
          }
      }
    }

    sl
  }

  private def stopped(): Unit = Swing.onEDT {
    serverPane.server = None
    meter.foreach { m =>
      meter = None
      // m.dispose()
    }
    onlinePane.foreach { p =>
      onlinePane = None
      boxPane.contents.remove(boxPane.contents.indexOf(p))
      // resizable = false
      pack()
    }
  }

  auralSystem.addClient(new AuralSystem.Client {
    def started(s: Server): Unit = me.started(s)
    def stopped()         : Unit = me.stopped()
  })
  // XXX TODO: removeClient

  title           = "Aural System" // Mellite.name
  closeOperation  = Window.CloseIgnore

  pack()
  front()
}