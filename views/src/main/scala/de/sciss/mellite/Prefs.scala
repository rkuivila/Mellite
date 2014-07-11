/*
 *  Prefs.scala
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

package de.sciss.mellite

import de.sciss.desktop.Preferences
import Preferences.{Entry, Type}
import de.sciss.file._
import javax.swing.UIManager
import UIManager.LookAndFeelInfo
import de.sciss.osc
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
  final val defaultAudioNumOutputs  = 8
  final val defaultAudioNumPrivate  = 512
  final val defaultHeadphonesBus    = 0

  def superCollider  : Entry[File  ] = userPrefs("supercollider"    )
  def audioDevice    : Entry[String] = userPrefs("audio-device"     )
  def audioNumOutputs: Entry[Int   ] = userPrefs("audio-num-outputs")
  def audioNumPrivate: Entry[Int   ] = userPrefs("audio-num-private")
  def headphonesBus  : Entry[Int   ] = userPrefs("headphones-bus"   )

  // ---- sensor ----

  final val defaultSensorProtocol = osc.UDP: osc.Transport.Net
  final val defaultSensorPort     = 0x4D6C  // "Ml"

  def sensorProtocol : Entry[osc.Transport.Net] = userPrefs("sensor-protocol")
  def sensorPort     : Entry[Int              ] = userPrefs("sensor-port"    )
}