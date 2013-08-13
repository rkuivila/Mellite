/*
 *  DynamicComponentImpl.scala
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

package de.sciss.mellite
package gui
package impl

import java.awt.event.{ComponentEvent, ComponentListener, WindowListener, WindowEvent}
import javax.swing.event.{AncestorEvent, AncestorListener}
import java.awt
import scala.swing.Component

// XXX TODO: replace with desktop version (however that needs component method)
trait DynamicComponentImpl {
  _: Component =>

  private var listening   = false
  private var win         = Option.empty[awt.Window]

  protected def componentShown (): Unit
  protected def componentHidden(): Unit

  final def isListening = listening

  peer.addAncestorListener(listener)
  learnWindow(Option(peer.getTopLevelAncestor))

  private def startListening(): Unit =
    if (!listening) {
      listening = true
      componentShown()
    }

  private def stopListening(): Unit =
    if (listening) {
      listening = false
      componentHidden()
    }

  private def forgetWindow(): Unit =
    win.foreach { w =>
      w.removeWindowListener(listener)
      w.removeComponentListener(listener)
      win = None
      stopListening()
    }

  private def learnWindow(c: Option[awt.Container]): Unit =
    c match {
      case Some(w: awt.Window) =>
        win = Some(w)
        w.addWindowListener(listener)
        w.addComponentListener(listener)
        if (w.isShowing) startListening()

      case _ =>
    }

  private object listener extends WindowListener with ComponentListener with AncestorListener {
    def windowOpened     (e: WindowEvent): Unit = startListening()
 		def windowClosed     (e: WindowEvent): Unit = stopListening ()

    def windowClosing    (e: WindowEvent) = ()
    def windowIconified  (e: WindowEvent) = ()
    def windowDeiconified(e: WindowEvent) = ()
    def windowActivated  (e: WindowEvent) = ()
    def windowDeactivated(e: WindowEvent) = ()

    def componentShown  (e: ComponentEvent): Unit = startListening()
    def componentHidden (e: ComponentEvent): Unit = stopListening ()

    def componentResized(e: ComponentEvent) = ()
    def componentMoved  (e: ComponentEvent) = ()

    def ancestorAdded(e: AncestorEvent): Unit = {
      val c = Option(e.getComponent.getTopLevelAncestor)
      if (c != win) {
        forgetWindow()
        learnWindow(c)
      }
    }

    def ancestorRemoved(e: AncestorEvent): Unit = forgetWindow()

    def ancestorMoved  (e: AncestorEvent) = ()
  }

//	def remove(): Unit = {
//		removeAncestorListener(listener)
//		forgetWindow()
//	}
}