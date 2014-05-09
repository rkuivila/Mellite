/*
 *  MainFrame.scala
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

import desktop.Window
import scala.swing.{CheckBox, Button, FlowPanel, ToggleButton, Action, Label, Slider, Component, Orientation, BoxPanel, Swing}
import de.sciss.synth.proc.AuralSystem
import de.sciss.synth.swing.{AudioBusMeter, ServerStatusPanel}
import de.sciss.synth.{addToTail, SynthDef, addToHead, AudioBus}
import Swing._
import java.awt.{Color, Font}
import scala.swing.event.{ButtonClicked, ValueChanged}
import collection.breakOut
import javax.swing.border.Border
import de.sciss.lucre.synth.{Txn, Server}
import de.sciss.file._
import de.sciss.mellite.gui.impl.WindowImpl
import scala.concurrent.stm.atomic
import de.sciss.lucre.swing.deferTx
import de.sciss.mellite.sensor.SensorSystem
import de.sciss.lucre.stm.TxnLike

final class MainFrame extends WindowImpl { me =>
  import Mellite.{auralSystem, sensorSystem}

  private val lbSensors = new Label("Sensors:")
  private val lbAudio   = new Label("Audio:")

  locally {
    val p1 = lbSensors.preferredSize
    val p2 = lbAudio  .preferredSize
    p1.width = math.max(p1.width, p2.width)
    p2.width = p1.width
    lbSensors.preferredSize = p1
    lbAudio  .preferredSize = p2
  }

  private val ggDumpSensors: CheckBox = new CheckBox("Dump") {
    listenTo(this)
    reactions += {
      case ButtonClicked(_) =>
        val dumpMode = if (selected) osc.Dump.Text else osc.Dump.Off
        atomic { implicit itx =>
          implicit val tx = TxnLike.wrap(itx)
          sensorSystem.serverOption.foreach { s =>
            // println(s"dump($dumpMode)")
            s.dump(dumpMode)
          }
        }
    }
    enabled = false
  }

  private val actionStartStopSensors: Action = new Action("Start") {
    def apply(): Unit = {
      val isRunning = atomic { implicit itx =>
        implicit val tx = TxnLike.wrap(itx)
        sensorSystem.serverOption.isDefined
      }
      if (isRunning) stopSensorSystem() else startSensorSystem()
    }
  }

  private val sensorServerPane = new BoxPanel(Orientation.Horizontal) {
    contents += HStrut(4)
    contents += lbSensors
    contents += HStrut(4)
    contents += new Button(actionStartStopSensors) {
      focusable = false
    }
    contents += HStrut(16)
    contents += ggDumpSensors
    contents += HGlue
  }

  private val audioServerPane = new ServerStatusPanel()
  audioServerPane.bootAction = Some(startAuralSystem _)

  private val boxPane = new BoxPanel(Orientation.Vertical)
  boxPane.contents += sensorServerPane
  boxPane.contents += new BoxPanel(Orientation.Horizontal) {
    contents += HStrut(4)
    contents += lbAudio
    contents += audioServerPane
  }

  // component.peer.getRootPane.putClientProperty("Window.style", "small")
  component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
  resizable = false
  contents  = boxPane

  def startAuralSystem(): Unit = {
    val config        = Server.Config()
    val programPath   = Prefs.superCollider.getOrElse(Prefs.defaultSuperCollider)
    if (programPath != Prefs.defaultSuperCollider) config.program = programPath.path
    val audioDevice   = Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice)
    if (audioDevice != Prefs.defaultAudioDevice) config.deviceName = Some(audioDevice)
    config.outputBusChannels = Prefs.audioNumOutputs.getOrElse(Prefs.defaultAudioNumOutputs)
    config.transport  = osc.TCP
    config.pickPort()

    atomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      auralSystem.start(config)
    }
  }

  private var meter       = Option.empty[AudioBusMeter]
  private var onlinePane  = Option.empty[Component]

    // val fnt0  = UIManager.getFont("Slider.font", Locale.US)
  private val smallFont   = // if (fnt0 != null)
      // fnt0.deriveFont(math.min(fnt0.getSize2D, 9.5f))
    // else
      new Font("SansSerif", Font.PLAIN, 9)

  // private val tinyFont = new Font("SansSerif", Font.PLAIN, 7)

  private def auralSystemStarted(s: Server)(implicit tx: Txn): Unit = {
    log("MainFrame: AuralSystem started")
    // XXX TODO: dirty dirty
    for(_ <- 1 to 4) s.nextNodeID()

    deferTx {
      import synth.Ops._

      audioServerPane.server = Some(s.peer)
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
        val ceil      = -0.2.dbamp
        val mainLim   = Limiter.ar(mainIn, level = ceil)
        val lim       = Lag.ar(K2A.ar("limiter".kr(1) * 2 - 1))
        val mainOut   = LinXFade2.ar(mainIn, mainLim, pan = lim)
        val hpBusL    = "hp-bus".kr(0)
        val hpBusR    = hpBusL + 1
        val hpAmp     = Lag.ar(K2A.ar("hp-amp".kr(1)))
        val hpInL     = Mix.tabulate((numOuts + 1) / 2)(i => in \ (i * 2))
        val hpInR     = Mix.tabulate( numOuts      / 2)(i => in \ (i * 2 + 1))
        val hpLimL    = Limiter.ar(hpInL * hpAmp, level = ceil)
        val hpLimR    = Limiter.ar(hpInR * hpAmp, level = ceil)

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

  private def auralSystemStopped()(implicit tx: Txn): Unit = {
    log("MainFrame: AuralSystem stopped")
    deferTx {
      audioServerPane.server = None
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
  }

  def stopSensorSystem(): Unit =
    atomic { implicit itx =>
      sensorSystem.stop()
    }

  def startSensorSystem(): Unit = {
    val config = SensorSystem.defaultConfig
    atomic { implicit itx =>
      implicit val tx = TxnLike.wrap(itx)
      sensorSystem.start(config)
    }
  }

  private def sensorSystemStarted(s: SensorSystem.Server)(implicit tx: TxnLike): Unit = {
    log("MainFrame: SensorSystem started")
    deferTx {
      actionStartStopSensors.title = "Stop"
      ggDumpSensors.enabled = true
    }
  }

  private def sensorSystemStopped()(implicit tx: TxnLike): Unit = {
    log("MainFrame: SensorSystem stopped")
    deferTx {
      actionStartStopSensors.title = "Start"
      ggDumpSensors.enabled   = false
      ggDumpSensors.selected  = false
    }
  }

  atomic { implicit itx =>
    implicit val tx = Txn.wrap(itx)
    auralSystem.addClient(new AuralSystem.Client {
      def started(s: Server)(implicit tx: Txn): Unit = me.auralSystemStarted(s)
      def stopped()         (implicit tx: Txn): Unit = me.auralSystemStopped()
    })
    sensorSystem.addClient(new SensorSystem.Client {
      def started(s: SensorSystem.Server)(implicit tx: TxnLike): Unit = me.sensorSystemStarted(s)
      def stopped()                      (implicit tx: TxnLike): Unit = me.sensorSystemStopped()
    })
  }
  // XXX TODO: removeClient

  title           = Mellite.name
  closeOperation  = Window.CloseIgnore

  pack()
  front()
}