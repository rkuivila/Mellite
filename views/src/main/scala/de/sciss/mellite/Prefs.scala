/*
 *  Prefs.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import de.sciss.desktop.Preferences
import Preferences.{Entry, Type}
import de.sciss.file._
import javax.swing.UIManager
import UIManager.LookAndFeelInfo
import de.sciss.osc
import de.sciss.synth.proc.SensorSystem
import scala.util.{Success, Try}

object Prefs {
  import Application.userPrefs

  implicit object LookAndFeelType extends Type[LookAndFeelInfo] {
    def toString(value: LookAndFeelInfo): String = value.getClassName
    def valueOf(string: String): Option[LookAndFeelInfo] =
      UIManager.getInstalledLookAndFeels.find(_.getClassName == string)
  }

  implicit object OSCProtocolType extends Type[osc.Transport.Net] {
    def toString(value: osc.Transport.Net): String = value.name
    def valueOf(string: String): Option[osc.Transport.Net] =
      Try(osc.Transport(string)) match {
        case Success(net: osc.Transport.Net) => Some(net)
        case _ => None
      }
  }

  // ---- system ----

  final val defaultDbLockTimeout = 1000
  def dbLockTimeout: Entry[Int] = userPrefs("lock-timeout")

  // ---- gui ----

  def defaultLookAndFeel: LookAndFeelInfo =
    UIManager.getInstalledLookAndFeels.find(_.getName == "Web Look And Feel").getOrElse {
      val clazzName = UIManager.getSystemLookAndFeelClassName
      LookAndFeelType.valueOf(clazzName).getOrElse(new LookAndFeelInfo("<system>", clazzName))
    }

  def lookAndFeel: Entry[LookAndFeelInfo] = userPrefs("look-and-feel")

  def nativeWindowDecoration: Entry[Boolean] = userPrefs("native-window-decoration")

  // ---- audio ----

  final val defaultSuperCollider    = file("<SC_HOME>")
  final val defaultAudioDevice      = "<default>"
  final val defaultAudioSampleRate  = 0

  final val defaultAudioBlockSize   = 64
  final val defaultAudioNumInputs   = 8
  final val defaultAudioNumOutputs  = 8
  final val defaultAudioNumPrivate  = 512
  final val defaultAudioNumWireBufs = 256
  final val defaultAudioMemorySize  = 64

  final val defaultHeadphonesBus    = 0

  def superCollider   : Entry[File   ] = userPrefs("supercollider"      )
  def audioDevice     : Entry[String ] = userPrefs("audio-device"       )
  def audioNumInputs  : Entry[Int    ] = userPrefs("audio-num-inputs"   )
  def audioNumOutputs : Entry[Int    ] = userPrefs("audio-num-outputs"  )
  def audioSampleRate : Entry[Int    ] = userPrefs("audio-sample-rate"  )

  def audioBlockSize  : Entry[Int    ] = userPrefs("audio-block-size"   )
  def audioNumPrivate : Entry[Int    ] = userPrefs("audio-num-private"  )
  def audioNumWireBufs: Entry[Int    ] = userPrefs("audio-num-wire-bufs")
  def audioMemorySize : Entry[Int    ] = userPrefs("audio-memory-size"  )

  def headphonesBus   : Entry[Int    ] = userPrefs("headphones-bus"     )
  def audioAutoBoot   : Entry[Boolean] = userPrefs("audio-auto-boot"    )

  // ---- sensor ----

  final val defaultSensorProtocol = osc.UDP: osc.Transport.Net
  def defaultSensorPort     = SensorSystem.defaultPort // 0x4D6C  // "Ml"
  def defaultSensorCommand  = SensorSystem.defaultCommand
  final val defaultSensorChannels = 16

  def sensorProtocol : Entry[osc.Transport.Net] = userPrefs("sensor-protocol")
  def sensorPort     : Entry[Int              ] = userPrefs("sensor-port"    )
  def sensorCommand  : Entry[String           ] = userPrefs("sensor-command" )
  def sensorChannels : Entry[Int              ] = userPrefs("sensor-channels")
  def sensorAutoStart: Entry[Boolean          ] = userPrefs("sensor-auto-start")

  // ---- sub-applications ----
  // they are here, because right now Mellite is `DelayedInit` which
  // can cause trouble with `var`s.

  /** The master volume in decibels. A value of -72 or less
    * is mapped to -inf.
    */
  def audioMasterVolume   : Entry[Int] = userPrefs("audio-master-volume")
  def headphonesVolume    : Entry[Int] = userPrefs("headphones-volume")

  def audioMasterLimiter  : Entry[Boolean]  = userPrefs("audio-master-limiter")
  def audioMasterPostMeter: Entry[Boolean]  = userPrefs("audio-master-post-meter")
  def headphonesActive    : Entry[Boolean]  = userPrefs("headphones-active")

  /** Whether to create a log (post) window or not. Defaults to `true`. */
  var useLogFrame: Boolean = true

  /** Whether to create a bus meters for the audio server or not. Defaults to `true`. */
  var useAudioMeters: Boolean = true

  /** Whether to create a meters for the sensors or not. Defaults to `true`. */
  var useSensorMeters: Boolean = true
}