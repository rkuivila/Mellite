/*
 *  ActionNewWorkspace.scala
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

import scala.swing.{Label, Dialog, Action}
import de.sciss.desktop.{OptionPane, FileDialog, KeyStrokes}
import util.control.NonFatal
import scala.swing.event.Key
import de.sciss.file._

object ActionNewWorkspace extends Action("Workspace...") {

  import KeyStrokes._

  accelerator = Some(menu1 + Key.N)

  private def deleteRecursive(f: File): Boolean = {
    if (f.isDirectory) {
      f.listFiles().foreach { f1 =>
        if (!deleteRecursive(f1)) return false
      }
    }
    f.delete()
  }

  private def fullTitle = "New Workspace"

  def apply(): Unit = {
    val tpeMessage = new Label( """<HTML><BODY><B>Workspaces can be confluent or ephemeral.</B><P><br>
        |A <I>confluent</I> workspace keeps a trace of its history.<P><br>
        |An <I>ephemeral</I> workspace does not remember its history.
        |""".stripMargin
    )

    val tpeEntries  = Seq("Confluent", "Ephemeral")
    val tpeInitial  = tpeEntries.headOption
    val tpeDlg      = OptionPane(message = tpeMessage, entries = tpeEntries, initial = tpeInitial)
    tpeDlg.title    = fullTitle
    val tpeRes      = tpeDlg.show().id
    if (tpeRes < 0) return
    val confluent   = tpeRes != 1

    val fileDlg = FileDialog.save(title = "Location for New Workspace")
    fileDlg.show(None).foreach { folder0 =>
      val name    = folder0.getName
      val folder  = if (name.endsWith(s".${Workspace.ext}")) folder0 else folder0.parent / s"$name.${Workspace.ext}"
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
        if (confluent) {
          val doc = Workspace.Confluent.empty(folder)
          // XXX TODO: SetFile -a E <folder>
          ActionOpenWorkspace.openGUI(doc)
        } else {
          val doc = Workspace.Ephemeral.empty(folder)
          ActionOpenWorkspace.openGUI(doc)
        }

      } catch {
        case NonFatal(e) =>
          Dialog.showMessage(
            message     = s"Unable to create new document ${folder.getPath} \n\n${formatException(e)}",
            title       = fullTitle,
            messageType = Dialog.Message.Error
          )
      }
    }
  }
}
