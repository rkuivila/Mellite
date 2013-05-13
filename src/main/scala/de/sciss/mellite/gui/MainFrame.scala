package de.sciss
package mellite
package gui

import desktop.impl.WindowImpl
import desktop.{Window, WindowHandler}
import synth.swing.j.JServerStatusPanel
import scala.swing.{Swing, Component}
import de.sciss.synth.proc.{Server, AuralSystem}

final class MainFrame extends WindowImpl {
  import Mellite.auralSystem

  def handler: WindowHandler = Mellite.windowHandler

  protected def style: Window.Style = Window.Regular

  private val serverPane = new JServerStatusPanel()
  serverPane.bootAction = Some(boot _)

  //  def setServer(s: Option[Server]) {
  //    serverPane.server = s.map(_.peer)
  //  }

  component.peer.getRootPane.putClientProperty("Window.style", "small")
  component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", java.lang.Boolean.TRUE)
  resizable = false
  contents  = Component.wrap(serverPane)

  private def boot() {
    val config        = Server.Config()
    val audioDevice   = Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice)
    if (audioDevice != Prefs.defaultAudioDevice) config.deviceName = Some(audioDevice)
    auralSystem.start(config)
  }

  private def setServer(s: Option[Server]) {
    Swing.onEDT(serverPane.server = s.map(_.peer))
  }

  auralSystem.addClient(new AuralSystem.Client {
    def started(s: Server) {
      setServer(Some(s))
    }

    def stopped() {
      setServer(None)
    }
  })
  // XXX TODO: removeClient

  title           = "Aural System" // Mellite.name
  closeOperation  = Window.CloseIgnore

  pack()
  front()
}