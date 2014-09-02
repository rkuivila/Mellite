/*
 *  Mellite.scala
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

import de.sciss.lucre.synth.{Txn, Server}
import de.sciss.mellite.gui.{DocumentViewHandler, LogFrame, MainFrame, MenuBar}
import de.sciss.desktop.impl.{SwingApplicationImpl, WindowHandlerImpl}
import de.sciss.desktop.{OptionPane, WindowHandler}
import de.sciss.synth.proc.{SensorSystem, AuralSystem}
import de.sciss.lucre.event.Sys
import javax.swing.UIManager
import scala.concurrent.stm.TxnExecutor
import scala.swing.Label
import scala.util.control.NonFatal
import com.alee.laf.checkbox.WebCheckBoxStyle
import com.alee.laf.progressbar.WebProgressBarStyle
import java.awt.Color

object Mellite extends SwingApplicationImpl("Mellite") {
  type Document = mellite.Workspace[_ <: Sys[_]]

  // lucre.event    .showLog = true
  // lucre.confluent.showLog = true
  // proc.showLog            = true
  // showLog                 = true
  // showTimelineLog         = true
  // proc.showAuralLog       = true
  // proc.showTransportLog   = true

  override lazy val windowHandler: WindowHandler = new WindowHandlerImpl(this, menuFactory) {
    override lazy val usesInternalFrames = {
      false // XXX TODO: eventually a preferences entry
    }

    override lazy val usesNativeDecoration = Prefs.nativeWindowDecoration.getOrElse(true)
  }

  protected def menuFactory = MenuBar.instance

  private lazy val _aural   = AuralSystem ()
  private lazy val _sensor: SensorSystem = SensorSystem()

  implicit def auralSystem : AuralSystem  = _aural
  implicit def sensorSystem: SensorSystem = _sensor

  /** Tries to start the aural system by booting SuperCollider.
    * This reads the relevant preferences entries such as
    * path, audio device, number of output channels, etc.
    * Transport is hard-coded to TCP at the moment, and port
    * is randomly picked.
    *
    * If the program path does not denote an existing file,
    * an error dialog is shown, and the method simply returns `false`.
    *
    * ''Note:'' This method must run on the EDT.
    *
    * @return `true` if the attempt to boot was made, `false` if the program was not found
    */
  def startAuralSystem(): Boolean = {
    lucre.swing.requireEDT()
    import de.sciss.file._
    val config        = Server.Config()
    val programPath   = Prefs.superCollider.getOrElse(Prefs.defaultSuperCollider)
    if (programPath != Prefs.defaultSuperCollider) config.program = programPath.path

    val f = file(config.program)
    if (!f.isFile && (f.parentOption.nonEmpty || {
      sys.env.getOrElse("PATH", "").split(File.pathSep).forall(p => !(file(p) / config.program).isFile)
    })) {
      val msg = new Label(
        s"""<HTML><BODY><B>The SuperCollider server program 'scsynth'<BR>
           |is not found at this location:</B><P>&nbsp;<BR>${config.program}<P>&nbsp;<BR>
           |Please adjust the path in the preferences.</BODY>""".stripMargin
      )
      val opt = OptionPane.message(msg, OptionPane.Message.Error)
      opt.show(title = "Starting Aural System")
      return false
    }

    val audioDevice   = Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice)
    if (audioDevice != Prefs.defaultAudioDevice) config.deviceName = Some(audioDevice)
    val numOutputs    = Prefs.audioNumOutputs.getOrElse(Prefs.defaultAudioNumOutputs)
    config.outputBusChannels = numOutputs
    val numPrivate    = Prefs.audioNumPrivate.getOrElse(Prefs.defaultAudioNumPrivate)
    config.audioBusChannels = numOutputs + numPrivate
    config.wireBuffers = math.max(256, numOutputs * 4)  // XXX TODO - sensible?
    config.transport  = osc.TCP
    config.pickPort()

    TxnExecutor.defaultAtomic { implicit itx =>
      implicit val tx = Txn.wrap(itx)
      auralSystem.start(config)
    }
    true
  }

  override protected def init(): Unit = {
    Application.init(this)

    // ---- type extensions ----

    mellite.initTypes()

    // ---- look and feel

    try {
      val web = "com.alee.laf.WebLookAndFeel"
      UIManager.installLookAndFeel("Web Look And Feel", web)
      UIManager.setLookAndFeel(Prefs.lookAndFeel.getOrElse(Prefs.defaultLookAndFeel).getClassName)
    } catch {
      case NonFatal(_) =>
    }
    // work-around for web-laf bug #118
    new javax.swing.JSpinner
    // some custom web-laf settings
    WebCheckBoxStyle   .animated            = false
    WebProgressBarStyle.progressTopColor    = Color.lightGray
    WebProgressBarStyle.progressBottomColor = Color.gray
    // XXX TODO: how to really turn of animation?
    WebProgressBarStyle.highlightWhite      = new Color(255, 255, 255, 0)
    WebProgressBarStyle.highlightDarkWhite  = new Color(255, 255, 255, 0)

    LogFrame           .instance    // init
    DocumentHandler    .instance    // init
    DocumentViewHandler.instance    // init

    new MainFrame
  }
}
