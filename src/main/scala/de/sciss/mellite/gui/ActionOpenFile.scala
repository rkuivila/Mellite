/*
 *  ActionOpenFile.scala
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

package de.sciss.mellite
package gui

import swing.{Dialog, Action}
import java.awt.event.KeyEvent
import de.sciss.synth.proc.Sys

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
