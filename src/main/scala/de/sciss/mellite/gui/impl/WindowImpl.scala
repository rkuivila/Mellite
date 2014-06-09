package de.sciss.mellite
package gui
package impl

import de.sciss.desktop
import de.sciss.desktop.WindowHandler
import scala.swing.Action
import de.sciss.pdflitz

trait WindowImpl extends desktop.impl.WindowImpl {
  def handler: WindowHandler = Application.windowHandler

  // bindMenu("actions.window-shot", Action(null)(windowShot()))

  //  private def windowShot(): Unit = {
  //    new pdflitz.SaveAction(contents.map(x => x: pdflitz.Generate.Source)).apply()
  //  }
}