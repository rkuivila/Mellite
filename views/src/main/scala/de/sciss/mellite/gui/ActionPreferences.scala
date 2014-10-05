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

import de.sciss.desktop.{PrefsGUI, Desktop, OptionPane, KeyStrokes}
import de.sciss.swingplus.{GroupPanel, Separator}
import javax.swing.UIManager
import scala.swing.Action
import scala.swing.event.Key
import de.sciss.mellite.Prefs
import de.sciss.file._

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
      title = "SuperCollider Server Location (scsynth)", accept = { f =>
        val f2 = if (Desktop.isMac && f.ext == "app") {
          val f1 = f / "Contents" / "Resources" / "scsynth"
          if (f1.exists) f1 else f
        } else f
        Some(f2)
      })

    val lbAutoBoot      = label("Automatic Boot")
    val ggAutoBoot      = checkBox(Prefs.autoBoot, default = false)

    val lbAudioDevice   = label("Audio Device")
    val ggAudioDevice   = textField(Prefs.audioDevice    , Prefs.defaultAudioDevice     )
    val lbNumInputs     = label("Input Channels")
    val ggNumInputs     = intField(Prefs.audioNumInputs  , Prefs.defaultAudioNumInputs  )
    val lbNumOutputs    = label("Output Channels")
    val ggNumOutputs    = intField(Prefs.audioNumOutputs , Prefs.defaultAudioNumOutputs )
    val lbSampleRate    = label("Sample Rate")
    val ggSampleRate    = intField(Prefs.audioSampleRate , Prefs.defaultAudioSampleRate, max = 384000)

    val sepAudioAdvanced = Separator()

    val lbBlockSize     = label("Block Size")
    val ggBlockSize     = intField(Prefs.audioBlockSize  , Prefs.defaultAudioBlockSize, min = 1)
    val lbNumPrivate    = label("Private Channels")
    val ggNumPrivate    = intField(Prefs.audioNumPrivate , Prefs.defaultAudioNumPrivate, min = 4)
    val lbNumWireBufs   = label("Wire Buffers")
    val ggNumWireBufs   = intField(Prefs.audioNumWireBufs, Prefs.defaultAudioNumWireBufs, min = 4, max = 262144)

    val sepAudioHeadphones = Separator()

    val lbHeadphones    = label("Headphones Bus")
    val ggHeadphones    = intField(Prefs.headphonesBus   , Prefs.defaultHeadphonesBus   )

    // ---- sensor ----
    val sepSensor = Separator()

    val lbSensorProtocol = label("Sensor Protocol")
    val ggSensorProtocol = combo(Prefs.sensorProtocol, Prefs.defaultSensorProtocol, Seq(osc.UDP, osc.TCP))(_.name)

    val lbSensorPort    = label("Sensor Port")
    val ggSensorPort    = intField(Prefs.sensorPort, Prefs.defaultSensorPort)

    val lbSensorCommand = label("Sensor Command")
    val ggSensorCommand = textField(Prefs.sensorCommand, Prefs.defaultSensorCommand)

    val lbSensorChannels = label("Sensor Channels")
    val ggSensorChannels = intField(Prefs.sensorChannels, Prefs.defaultSensorChannels)

    // ---- panel ----

    val box = new GroupPanel {
      // val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
      horizontal = Par(sepAudio, sepSensor, sepAudioAdvanced, sepAudioHeadphones, Seq(
        Par(lbLookAndFeel, lbNativeDecoration, lbSuperCollider, lbAutoBoot, lbAudioDevice, lbNumInputs, lbNumOutputs,
          lbSampleRate, lbBlockSize, lbNumPrivate, lbNumWireBufs, lbHeadphones, lbSensorProtocol, lbSensorPort,
          lbSensorCommand, lbSensorChannels),
        Par(ggLookAndFeel, ggNativeDecoration, ggSuperCollider, ggAutoBoot, ggAudioDevice, ggNumInputs, ggNumOutputs,
          ggSampleRate, ggNumPrivate, ggBlockSize, ggNumWireBufs, ggHeadphones, ggSensorProtocol, ggSensorPort,
          ggSensorCommand, ggSensorChannels)
      ))
      vertical = Seq(
        Par(Baseline)(lbLookAndFeel     , ggLookAndFeel     ),
        Par(Baseline)(lbNativeDecoration, ggNativeDecoration),
        sepAudio,
        Par(Baseline)(lbSuperCollider   , ggSuperCollider   ),
        Par(Baseline)(lbAutoBoot        , ggAutoBoot        ),
        Par(Baseline)(lbAudioDevice     , ggAudioDevice     ),
        Par(Baseline)(lbNumInputs       , ggNumInputs       ),
        Par(Baseline)(lbNumOutputs      , ggNumOutputs      ),
        Par(Baseline)(lbSampleRate      , ggSampleRate      ),
        sepAudioAdvanced,
        Par(Baseline)(lbBlockSize       , ggBlockSize       ),
        Par(Baseline)(lbNumPrivate      , ggNumPrivate      ),
        Par(Baseline)(lbNumWireBufs     , ggNumWireBufs     ),
        sepAudioHeadphones,
        Par(Baseline)(lbHeadphones      , ggHeadphones      ),
        sepSensor,
        Par(Baseline)(lbSensorProtocol  , ggSensorProtocol  ),
        Par(Baseline)(lbSensorPort      , ggSensorPort      ),
        Par(Baseline)(lbSensorCommand   , ggSensorCommand   ),
        Par(Baseline)(lbSensorChannels  , ggSensorChannels  )
      )
    }

    val opt   = OptionPane.message(message = box, messageType = OptionPane.Message.Plain)
    opt.title = "Preferences"
    opt.show(None)
  }
}