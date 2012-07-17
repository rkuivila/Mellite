package de.sciss.mellite
package gui

import swing.Action
import java.awt.event.KeyEvent

object ActionOpenFile extends Action( "Open...") {
   accelerator = Some( primaryMenuKey( KeyEvent.VK_O ))

   def apply() {
//      val doc = Document.em
   }
}
