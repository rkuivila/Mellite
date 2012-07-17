package de.sciss.mellite
package gui

import swing.Action
import java.awt.event.KeyEvent

object ActionNewFile extends Action( "New...") {
   accelerator = Some( primaryMenuKey( KeyEvent.VK_N ))

   def apply() {
      FileDialog.save( title = "Location for New Document" ).foreach { f =>
         println( "Aqui " + f )
      }
   }
}
