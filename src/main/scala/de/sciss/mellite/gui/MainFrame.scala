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
import synth.swing.j.JServerStatusPanel
import scala.swing.{Swing, Component}
import de.sciss.synth.proc.{Server, AuralSystem}

final class MainFrame extends WindowImpl {
  import Mellite.auralSystem

  def handler: WindowHandler = Mellite.windowHandler

  protected def style: Window.Style = Window.Regular

  private val serverPane = new JServerStatusPanel()
  serverPane.bootAction = Some(boot _)

  //  def setServer(s: Option[Server]): Unit serverPane.server = s.map(_.peer)

  component.peer.getRootPane.putClientProperty("Window.style", "small")
  component.peer.getRootPane.putClientProperty("apple.awt.brushMetalLook", true)
  resizable = false
  contents  = Component.wrap(serverPane)

  private def boot(): Unit = {
    val config        = Server.Config()
    val audioDevice   = Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice)
    if (audioDevice != Prefs.defaultAudioDevice) config.deviceName = Some(audioDevice)
    auralSystem.start(config)
  }

  private def setServer(s: Option[Server]): Unit =
    Swing.onEDT(serverPane.server = s.map(_.peer))

  auralSystem.addClient(new AuralSystem.Client {
    def started(s: Server): Unit = setServer(Some(s))
    def stopped()         : Unit = setServer(None)
  })
  // XXX TODO: removeClient

  title           = "Aural System" // Mellite.name
  closeOperation  = Window.CloseIgnore

  pack()
  front()
}