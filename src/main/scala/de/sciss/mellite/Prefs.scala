package de.sciss.mellite

import de.sciss.desktop.Preferences

object Prefs {
  import Mellite.userPrefs

  final val defaultAudioDevice = "<default>"

  def audioDevice: Preferences.Entry[String] = userPrefs[String]("audio-device")
}