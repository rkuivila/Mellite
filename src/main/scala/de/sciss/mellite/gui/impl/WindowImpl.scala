package de.sciss.mellite.gui.impl

import de.sciss.desktop
import de.sciss.desktop.WindowHandler
import de.sciss.mellite.{Mellite => App}
import scala.swing.Action
import de.sciss.pdflitz

trait WindowImpl extends desktop.impl.WindowImpl {
  def handler: WindowHandler = App.windowHandler

  bindMenu("actions.windowShot", Action(null)(windowShot()))

  private def windowShot(): Unit = {
    new pdflitz.SaveAction(contents.map(x => x: pdflitz.Generate.Source)).apply()
  }
}