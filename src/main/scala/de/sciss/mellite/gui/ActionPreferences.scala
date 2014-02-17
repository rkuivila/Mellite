/*
 *  ActionPreferences.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite
package gui

import scala.swing.{FlowPanel, Button, TextField, Alignment, Label, Action}
import java.awt.event.KeyEvent
import de.sciss.desktop.{FileDialog, Preferences, OptionPane, KeyStrokes}
import scalaswingcontrib.group.GroupPanel
import scala.swing.Swing.EmptyIcon
import scala.swing.event.{EditDone, ValueChanged}
import de.sciss.swingplus.Spinner
import javax.swing.{JPanel, SpinnerNumberModel}
import de.sciss.file._

object ActionPreferences extends Action("Preferences...") {
  import KeyStrokes._
  accelerator = Some(menu1 + KeyEvent.VK_COMMA)

  def apply(): Unit = {
    import language.reflectiveCalls

    def label(text: String) = new Label(text + ":", EmptyIcon, Alignment.Right)

    def spinner(prefs: Preferences.Entry[Int], default: => Int, min: Int = 0, max: Int = 65536,
                step: Int = 1): Spinner = {
      val m = new SpinnerNumberModel(prefs.getOrElse(default), min, max, step)
      new Spinner(m) {
        listenTo(this)
        reactions += {
          case ValueChanged(_) => value match{
            case i: Int => prefs.put(i)
            case _ => println(s"Unexpected value $value")
          }
        }
      }
    }

    val box = new GroupPanel {
      val lbSuperCollider = label("SuperCollider (scsynth)")
      val txSuperCollider = new TextField(Prefs.superCollider.getOrElse(Prefs.defaultSuperCollider).path, 16) {
        listenTo(this)
        reactions += {
          case EditDone(_) =>
            if (text.isEmpty) text = Prefs.defaultSuperCollider.path
            Prefs.superCollider.put(file(text))
        }
      }
      val btSuperCollider = Button("…") {
        val dlg = FileDialog.open(init = Prefs.superCollider.get, title = "SuperCollider Server Location (scsynth)")
        dlg.show(None).foreach { f =>
          txSuperCollider.text = f.path
          Prefs.superCollider.put(f)
        }
      }
      btSuperCollider.peer.putClientProperty("JButton.buttonType", "square")
      val ggSuperCollider = new FlowPanel(txSuperCollider, btSuperCollider) {
        override lazy val peer: JPanel =
          new JPanel(new java.awt.FlowLayout(java.awt.FlowLayout.TRAILING, 0, 0)) with SuperMixin {
            override def getBaseline(width: Int, height: Int): Int = {
              val res = txSuperCollider.peer.getBaseline(width, height)
              res + txSuperCollider.peer.getY
            }
          }
      }

      val lbAudioDevice   = label("Audio Device")
      val ggAudioDevice   = new TextField(Prefs.audioDevice.getOrElse(Prefs.defaultAudioDevice), 16) {
        listenTo(this)
        reactions += {
          case EditDone(_) =>
            if (text.isEmpty) text = Prefs.defaultAudioDevice
            Prefs.audioDevice.put(text)
        }
      }
      val lbNumOutputs    = label("Output Channels")
      val ggNumOutputs    = spinner(Prefs.audioNumOutputs, Prefs.defaultAudioNumOutputs)

      val lbHeadphones    = label("Headphones Bus")
      val ggHeadphones    = spinner(Prefs.headphonesBus, Prefs.defaultHeadphonesBus)

      // val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
      theHorizontalLayout is Sequential(
        Parallel(lbSuperCollider, lbAudioDevice, lbNumOutputs, lbHeadphones),
        Parallel(ggSuperCollider, ggAudioDevice, ggNumOutputs, ggHeadphones)
      )
      theVerticalLayout is Sequential(
        Parallel(Baseline)(lbSuperCollider, ggSuperCollider),
        Parallel(Baseline)(lbAudioDevice  , ggAudioDevice  ),
        Parallel(Baseline)(lbNumOutputs   , ggNumOutputs   ),
        Parallel(Baseline)(lbHeadphones   , ggHeadphones   )
      )
    }

    val opt   = OptionPane.message(message = box, messageType = OptionPane.Message.Plain)
    opt.title = "Preferences"
    opt.show(None)
  }
}