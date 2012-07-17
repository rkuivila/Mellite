package de.sciss.mellite
package gui

import swing.{Dialog, Action}
import java.awt.event.KeyEvent
import java.io.File

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

   private def formatException( e: Throwable ) : String = {
      e.getClass.toString + " : " + e.getMessage + "\n" +
      e.getStackTrace.take( 10 ).map( "   at " + _ ).mkString( "\n" )
   }

   private def initDoc( doc: Document ) {
      Dialog.showMessage(
         message = "Done.",
         title = fullTitle,
         messageType = Dialog.Message.Info
      )
   }

   def apply() {
      FileDialog.save( title = "Location for New Document" ).foreach { f =>
         if( f.exists() ) {
            if( Dialog.showConfirmation(
               message = "Document " + f.getPath + " already exists.\nAre you sure you want to overwrite it?",
               title = fullTitle,
               optionType = Dialog.Options.OkCancel,
               messageType = Dialog.Message.Warning
            ) != Dialog.Result.Ok ) return

            if( !deleteRecursive( f )) {
               Dialog.showMessage(
                  message = "Unable to delete existing document " + f.getPath,
                  title = fullTitle,
                  messageType = Dialog.Message.Error
               )
               return
            }
         }

         try {
            val doc = Document.empty( f )
            initDoc( doc )
         } catch {
            case e: Exception =>
               Dialog.showMessage(
                  message = "Unabled to create new document " + f.getPath + "\n\n" + formatException( e ),
                  title = fullTitle,
                  messageType = Dialog.Message.Error
               )
         }
      }
   }
}
