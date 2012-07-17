package de.sciss.mellite
package gui

import swing.{Dialog, Action}
import java.awt.event.KeyEvent
import java.io.File
import de.sciss.lucre.stm.Sys

object ActionNewFile extends Action( "New...") {
   accelerator = Some( primaryMenuKey( KeyEvent.VK_N ))

//   private def name : String = {
//      val str = this.peer.getValue( javax.swing.Action.NAME ).toString
//      if( str.endsWith( "..." )) str.substring( str.length - 3 ) else str
//   }

//   private def create( dir: File ) {
//
//   }

   private def deleteRecursive( f: File ) : Boolean = {
      if( f.isDirectory ) {
         f.listFiles().foreach { f1 =>
            if( !deleteRecursive( f1 )) return false
         }
      }
      f.delete()
   }

   private def fullTitle = "New Document"

   private def initDoc[ S <: Sys[ S ]]( doc: Document[ S ]) {
      doc.cursor.step { implicit tx =>
         DocumentFrame( doc )
      }
   }

   def apply() {
      FileDialog.save( title = "Location for New Document" ).foreach { folder =>
         if( folder.exists() ) {
            if( Dialog.showConfirmation(
               message = "Document " + folder.getPath + " already exists.\nAre you sure you want to overwrite it?",
               title = fullTitle,
               optionType = Dialog.Options.OkCancel,
               messageType = Dialog.Message.Warning
            ) != Dialog.Result.Ok ) return

            if( !deleteRecursive( folder )) {
               Dialog.showMessage(
                  message = "Unable to delete existing document " + folder.getPath,
                  title = fullTitle,
                  messageType = Dialog.Message.Error
               )
               return
            }
         }

         try {
            val doc = Document.empty( folder )
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
