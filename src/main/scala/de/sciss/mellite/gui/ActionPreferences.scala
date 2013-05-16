/*
 *  ActionPreferences.scala
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

import scala.swing.{TextField, Alignment, Label, Action}
import java.awt.event.KeyEvent
import de.sciss.desktop.{OptionPane, KeyStrokes}
import scalaswingcontrib.group.GroupPanel
import scala.swing.Swing.EmptyIcon
import scala.swing.event.ValueChanged

object ActionPreferences extends Action("Preferences...") {
  import KeyStrokes._
  accelerator = Some(menu1 + KeyEvent.VK_COMMA)

  def apply() {
    import language.reflectiveCalls
    val box = new GroupPanel {
      val lbAudioDevice = new Label("Audio Device:", EmptyIcon, Alignment.Right)
      val ggAudioDevice = new TextField(Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice), 16) {
        listenTo(this)
        reactions += {
          case ValueChanged(_) => Prefs.audioDevice.put(text)
        }
      }
      // val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
      theHorizontalLayout is Sequential(lbAudioDevice, ggAudioDevice)
      theVerticalLayout   is Parallel(Baseline)(lbAudioDevice, ggAudioDevice)
    }

    val opt   = OptionPane.message(message = box, messageType = OptionPane.Message.Plain)
    opt.title = "Preferences"
    opt.show(None)
  }
}
