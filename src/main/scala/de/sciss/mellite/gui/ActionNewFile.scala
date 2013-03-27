/*
 *  ActionNewFile.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
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
import java.io.File
import de.sciss.synth.proc.Sys
import de.sciss.desktop.KeyStrokes
import util.control.NonFatal

object ActionNewFile extends Action( "New...") {
  import KeyStrokes._
   accelerator = Some(menu1 + KeyEvent.VK_N)

  private def deleteRecursive(f: File): Boolean = {
    if (f.isDirectory) {
      f.listFiles().foreach {
        f1 =>
          if (!deleteRecursive(f1)) return false
      }
    }
    f.delete()
  }

  private def fullTitle = "New Document"

  private def initDoc[S <: Sys[S]](doc: Document[S]) {
    doc.cursor.step { implicit tx =>
      DocumentFrame(doc)
    }
  }

  def apply() {
    FileDialog.save(title = "Location for New Document").foreach { folder =>
      if (folder.exists()) {
        if (Dialog.showConfirmation(
          message = "Document " + folder.getPath + " already exists.\nAre you sure you want to overwrite it?",
          title = fullTitle,
          optionType = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Warning
        ) != Dialog.Result.Ok) return

        if (!deleteRecursive(folder)) {
          Dialog.showMessage(
            message = "Unable to delete existing document " + folder.getPath,
            title = fullTitle,
            messageType = Dialog.Message.Error
          )
          return
        }
      }

      try {
        val doc = Document.empty(folder)
        initDoc(doc)
      } catch {
        case NonFatal(e) =>
          Dialog.showMessage(
            message = "Unabled to create new document " + folder.getPath + "\n\n" + formatException(e),
            title = fullTitle,
            messageType = Dialog.Message.Error
          )
      }
    }
  }
}
