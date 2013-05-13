package de.sciss
package mellite
package gui

import desktop.impl.WindowImpl
import desktop.{Window, WindowHandler}
import synth.swing.j.JServerStatusPanel
import scala.swing.{Swing, Component}
import de.sciss.synth.proc.{Server, AuralSystem}

final class MainFrame extends WindowImpl {
  def handler: WindowHandler = Mellite.windowHandler

  protected def style: Window.Style = Window.Regular

  private val serverPane = new JServerStatusPanel()
  serverPane.bootAction = Some(() => Mellite.auralSystem.start())

  //  def setServer(s: Option[Server]) {
  //    serverPane.server = s.map(_.peer)
  //  }

  component.peer.getRootPane.putClientProperty("Window.style", "small")
  component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", java.lang.Boolean.TRUE)
  resizable = false
  contents  = Component.wrap(serverPane)

  private def setServer(s: Option[Server]) {
    Swing.onEDT(serverPane.server = s.map(_.peer))
  }

  Mellite.auralSystem.addClient(new AuralSystem.Client {
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