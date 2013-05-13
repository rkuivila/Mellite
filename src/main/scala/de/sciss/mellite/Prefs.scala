package de.sciss.mellite

import de.sciss.desktop.Preferences

object Prefs {
  import Mellite.userPrefs

  def audioDevice: Preferences.Entry[String] = userPrefs[String]("audio-device")
}