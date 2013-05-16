package de.sciss.mellite

import java.io.File

object IO {
  def revealInFinder(file: File) {
    val cmd = Seq("osascript", "-e", "tell application \"Finder\"", "-e", "activate", "-e",
      "open location \"file:" + file.parent + "\"", "-e", "select file \"" + file.name + "\" of folder of the front window",
      "-e", "end tell"
    )
    import sys.process._
    // println(cmd.mkString(" "))
    cmd.run()
  }
}