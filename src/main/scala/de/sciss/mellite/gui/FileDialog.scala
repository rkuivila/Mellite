/*
 *  FileDialog.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

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
