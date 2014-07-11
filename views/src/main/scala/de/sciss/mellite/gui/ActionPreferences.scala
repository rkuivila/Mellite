/*
 *  ActionPreferences.scala
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
package mellite.gui

import de.sciss.desktop.{OptionPane, KeyStrokes}
import de.sciss.swingplus.{GroupPanel, Separator}
import javax.swing.UIManager
import scala.swing.Action
import scala.swing.event.Key
import de.sciss.mellite.Prefs
import de.sciss.mellite.gui.impl.PrefsGUI

object ActionPreferences extends Action("Preferences...") {
  import KeyStrokes._

  accelerator = Some(menu1 + Key.Comma)

  def apply(): Unit = {
    import PrefsGUI._

    // ---- appearance ----

    val lbLookAndFeel   = label("Look-and-Feel")
    val ggLookAndFeel   = combo(Prefs.lookAndFeel, Prefs.defaultLookAndFeel,
      UIManager.getInstalledLookAndFeels)(_.getName)

    val lbNativeDecoration = label("Native Window Decoration")
    val ggNativeDecoration = checkBox(Prefs.nativeWindowDecoration, default = true)

    // ---- audio ----
    val sepAudio = Separator()

    val lbSuperCollider = label("SuperCollider (scsynth)")
    val ggSuperCollider = pathField(Prefs.superCollider, Prefs.defaultSuperCollider,
      title = "SuperCollider Server Location (scsynth)")

    val lbAudioDevice   = label("Audio Device")
    val ggAudioDevice   = textField(Prefs.audioDevice   , Prefs.defaultAudioDevice    )
    val lbNumOutputs    = label("Output Channels")
    val ggNumOutputs    = intField(Prefs.audioNumOutputs, Prefs.defaultAudioNumOutputs)
    val lbNumPrivate    = label("Private Channels")
    val ggNumPrivate    = intField(Prefs.audioNumPrivate, Prefs.defaultAudioNumPrivate)

    val lbHeadphones    = label("Headphones Bus")
    val ggHeadphones    = intField(Prefs.headphonesBus  , Prefs.defaultHeadphonesBus  )

    // ---- sensor ----
    val sepSensor = Separator()

    val lbSensorProtocol = label("Sensor Protocol")
    val ggSensorProtocol = combo(Prefs.sensorProtocol, Prefs.defaultSensorProtocol, Seq(osc.UDP, osc.TCP))(_.name)

    val lbSensorPort    = label("Sensor Port")
    val ggSensorPort    = intField(Prefs.sensorPort, Prefs.defaultSensorPort)

    // ---- panel ----

    val box = new GroupPanel {
      // val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
      horizontal = Par(sepAudio, sepSensor, Seq(
        Par(lbLookAndFeel, lbNativeDecoration, lbSuperCollider, lbAudioDevice, lbNumOutputs, lbNumPrivate, lbHeadphones, lbSensorProtocol, lbSensorPort),
        Par(ggLookAndFeel, ggNativeDecoration, ggSuperCollider, ggAudioDevice, ggNumOutputs, ggNumPrivate, ggHeadphones, ggSensorProtocol, ggSensorPort)
      ))
      vertical = Seq(
        Par(Baseline)(lbLookAndFeel     , ggLookAndFeel     ),
        Par(Baseline)(lbNativeDecoration, ggNativeDecoration),
        sepAudio,
        Par(Baseline)(lbSuperCollider   , ggSuperCollider   ),
        Par(Baseline)(lbAudioDevice     , ggAudioDevice     ),
        Par(Baseline)(lbNumOutputs      , ggNumOutputs      ),
        Par(Baseline)(lbNumPrivate      , ggNumPrivate      ),
        Par(Baseline)(lbHeadphones      , ggHeadphones      ),
        sepSensor,
        Par(Baseline)(lbSensorProtocol  , ggSensorProtocol  ),
        Par(Baseline)(lbSensorPort      , ggSensorPort      )
      )
    }

    val opt   = OptionPane.message(message = box, messageType = OptionPane.Message.Plain)
    opt.title = "Preferences"
    opt.show(None)
  }
}