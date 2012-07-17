package de.sciss.mellite
package gui

import swing.{Dialog, Action}
import java.awt.event.KeyEvent
import de.sciss.lucre.stm.Sys

object ActionOpenFile extends Action( "Open...") {
   accelerator = Some( primaryMenuKey( KeyEvent.VK_O ))

   private def fullTitle = "Open Document"

   private def initDoc[ S <: Sys[ S ]]( doc: Document[ S ]) {
      doc.cursor.step { implicit tx =>
         DocumentFrame( doc )
      }
   }

   def apply() {
      FileDialog.open( title = fullTitle ).foreach { f =>
         val folder  = f.getParentFile
         try {
            val doc     = Document.read( folder )
            initDoc( doc )
         } catch {
            case e: Exception =>
               Dialog.showMessage(
                  message = "Unabled to create new document " + folder.getPath + "\n\n" + formatException( e ),
                  title = fullTitle,
                  messageType = Dialog.Message.Error
               )
         }
      }
   }
}
