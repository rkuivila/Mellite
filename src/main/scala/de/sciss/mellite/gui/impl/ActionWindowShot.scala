/*
 *  ActionWindowShot.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import scala.swing.{Dimension, Graphics2D, Action}
import scala.swing.event.Key
import de.sciss.{desktop, pdflitz}

class ActionWindowShot(w: desktop.Window) extends Action("Window Screen-Shot") {

  import de.sciss.desktop.KeyStrokes
  import KeyStrokes._

  accelerator = Some(menu1 + shift + Key.P)

  def apply(): Unit = windowShot()

  private def windowShot(): Unit = {
    val c = new pdflitz.Generate.Source {
      import w.{component => comp}
      def render(g: Graphics2D): Unit = comp.peer.paint(g)

      def preferredSize: Dimension = comp.preferredSize

      def size: Dimension = comp.size
    }
    new pdflitz.SaveAction(c :: Nil).apply()
  }
}