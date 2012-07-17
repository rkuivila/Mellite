package de.sciss.mellite.gui

import java.io.File
import java.awt
import swing.Frame

object FileDialog {
   def save( frame: Option[ Frame ] = None, dir: Option[ File ] = None, file: Option[ String ] = None,
             title: String = "Save" ) : Option[ File ] = {
      val dlg = new awt.FileDialog( frame.map( _.peer ).orNull, title, awt.FileDialog.SAVE )
      dir.foreach { d => dlg.setDirectory( d.getPath )}
      file.foreach { f => dlg.setFile( f )}
      dlg.setVisible( true )
      val resDir  = dlg.getDirectory
      val resFile = dlg.getFile
      if( resDir != null && resFile != null ) Some( new File( resDir, resFile )) else None
   }
}
