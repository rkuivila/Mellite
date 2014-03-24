/*
 *  ActionNewFile.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
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
import de.sciss.desktop.{FileDialog, KeyStrokes}
import util.control.NonFatal

object ActionNewFile extends Action("Session...") {
  import KeyStrokes._
  accelerator = Some(menu1 + KeyEvent.VK_N)

  private def deleteRecursive(f: File): Boolean = {
    if (f.isDirectory) {
      f.listFiles().foreach { f1 =>
        if (!deleteRecursive(f1)) return false
      }
    }
    f.delete()
  }

  private def fullTitle = "New Document"

  def apply(): Unit =
    FileDialog.save(title = "Location for New Document").show(None).foreach { folder0 =>
      val name    = folder0.getName
      val folder  = if (name.endsWith(".mllt")) folder0 else new File(folder0.getParentFile, name + ".mllt")
      if (folder.exists()) {
        if (Dialog.showConfirmation(
          message     = s"Document ${folder.getPath} already exists.\nAre you sure you want to overwrite it?",
          title       = fullTitle,
          optionType  = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Warning
        ) != Dialog.Result.Ok) return

        if (!deleteRecursive(folder)) {
          Dialog.showMessage(
            message     = s"Unable to delete existing document ${folder.getPath}",
            title       = fullTitle,
            messageType = Dialog.Message.Error
          )
          return
        }
      }

      try {
        val doc = Document.empty(folder)
        // XXX TODO: SetFile -a E <folder>
        ActionOpenFile.openGUI(doc)

      } catch {
        case NonFatal(e) =>
          Dialog.showMessage(
            message     = s"Unabled to create new document ${folder.getPath} \n\n${formatException(e)}",
            title       = fullTitle,
            messageType = Dialog.Message.Error
          )
      }
    }
}
