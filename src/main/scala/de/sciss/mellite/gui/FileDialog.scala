package de.sciss.mellite.gui

import java.io.File
import java.awt
import swing.Frame

object FileDialog {
   def save( frame: Option[ Frame ] = None, dir: Option[ File ] = None, file: Option[ String ] = None,
             title: String = "Save" ) : Option[ File ] =
      show( awt.FileDialog.SAVE, frame, dir, file, title )

   def open( frame: Option[ Frame ] = None, dir: Option[ File ] = None, file: Option[ String ] = None,
             title: String = "Open" ) : Option[ File ] =
      show( awt.FileDialog.LOAD, frame, dir, file, title )

   private def show( dlgType: Int, frame: Option[ Frame ], dir: Option[ File ], file: Option[ String ], title: String ) : Option[ File ] = {
      val dlg = new awt.FileDialog( frame.map( _.peer ).orNull, title, dlgType )
      dir.foreach { d => dlg.setDirectory( d.getPath )}
      file.foreach { f => dlg.setFile( f )}
      dlg.setVisible( true )
      val resDir  = dlg.getDirectory
      val resFile = dlg.getFile
      if( resDir != null && resFile != null ) Some( new File( resDir, resFile )) else None
   }
}
