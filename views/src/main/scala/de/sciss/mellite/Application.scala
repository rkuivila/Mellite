package de.sciss.mellite

import de.sciss.mellite
import de.sciss.desktop.{SwingApplication, WindowHandler, Preferences}
import de.sciss.lucre.event.Sys

/** A proxy for a swing application. */
object Application extends SwingApplication { me =>

  type Document = mellite.Workspace[_ <: Sys[_]]

  private[this] var peer: SwingApplication { type Document = Application.Document } = null

  private[this] val sync = new AnyRef

  @inline private[this] def requireInitialized(): Unit =
    if (peer == null) throw new IllegalStateException("Application not yet initialized")

  def init(peer: SwingApplication { type Document = Application.Document }): Unit = sync.synchronized {
    if (me.peer != null) throw new IllegalStateException("Trying to initialize application twice")
    me.peer = peer
  }

  def name: String = {
    requireInitialized()
    peer.name
  }

  def userPrefs: Preferences = {
    requireInitialized()
    peer.userPrefs
  }

  def systemPrefs: Preferences = {
    requireInitialized()
    peer.systemPrefs
  }

  def documentHandler: DocumentHandler = {
    requireInitialized()
    peer.documentHandler
  }

  def windowHandler: WindowHandler = {
    requireInitialized()
    peer.windowHandler
  }

  def quit(): Unit = {
    requireInitialized()
    peer.quit()
  }

  def getComponent[A](key: String): Option[A] = {
    requireInitialized()
    peer.getComponent(key)
  }

  def addComponent(key: String, component: Any): Unit = {
    requireInitialized()
    peer.addComponent(key, component)
  }

  def removeComponent(key: String): Unit = {
    requireInitialized()
    peer.removeComponent(key)
  }
}
