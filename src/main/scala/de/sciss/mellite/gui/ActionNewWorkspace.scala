/*
 *  ActionNewWorkspace.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import java.util.concurrent.TimeUnit

import de.sciss.desktop.{FileDialog, KeyStrokes, OptionPane}
import de.sciss.file._
import de.sciss.lucre.stm.DataStore
import de.sciss.lucre.stm.store.BerkeleyDB
import de.sciss.synth.proc
import de.sciss.synth.proc.Workspace

import scala.concurrent.duration.Duration
import scala.swing.event.Key
import scala.swing.{Action, Dialog, Label}
import scala.util.control.NonFatal

object ActionNewWorkspace extends Action("Workspace...") {
  import KeyStrokes._

  accelerator = Some(menu1 + shift + Key.N)

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
        |An <I>ephemeral</I> workspace does not remember its history.<br>
        |An ephemeral workspace can either <I>durable</I> (stored on disk) or purely <I>in-memory</I>.
        |""".stripMargin
    )

    val tpeEntries  = Seq("Confluent", "Durable", "In-Memory")
    val tpeInitial  = tpeEntries.headOption
    val tpeDlg      = OptionPane(message = tpeMessage, entries = tpeEntries, initial = tpeInitial)
    tpeDlg.title    = fullTitle
    val tpeRes      = tpeDlg.show().id
    if (tpeRes < 0) return
    val confluent   = tpeRes == 0
    val inMemory    = tpeRes == 2

    if      (inMemory)  performInMemory()
    else if (confluent) performConfluent()
    else                performDurable()
  }

  def performInMemory(): Workspace.InMemory = {
    val doc = Workspace.InMemory()
    ActionOpenWorkspace.openGUI(doc)
    doc
  }

  def performDurable(): Option[Workspace.Durable] = create[proc.Durable, Workspace.Durable] { (folder, config) =>
    Workspace.Durable.empty(folder, config)
  }

  def performConfluent(): Option[Workspace.Confluent] = create[proc.Confluent, Workspace.Confluent] { (folder, config) =>
    Workspace.Confluent.empty(folder, config)
  }

  private def selectFile(): Option[File] = {
    val fileDlg = FileDialog.save(title = "Location for New Workspace")
    fileDlg.show(None).flatMap { folder0 =>
      val name    = folder0.name
      val folder  = if (folder0.ext.toLowerCase == Workspace.ext)
        folder0
      else
        folder0.parent / s"$name.${Workspace.ext}"

      if (!folder.exists()) Some(folder) else {
        val isOk = Dialog.showConfirmation(
          message     = s"Document ${folder.path} already exists.\nAre you sure you want to overwrite it?",
          title       = fullTitle,
          optionType  = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Warning
        ) != Dialog.Result.Ok

        if (!isOk) None else if (deleteRecursive(folder)) Some(folder) else {
          Dialog.showMessage(
            message     = s"Unable to delete existing document ${folder.path}",
            title       = fullTitle,
            messageType = Dialog.Message.Error
          )
          None
        }
      }
    }
  }

  private def create[S <: de.sciss.lucre.synth.Sys[S], A <: Workspace[S]](fun: (File, DataStore.Factory) => A): Option[A] =
    selectFile().flatMap { folder =>
      try {
        val config          = BerkeleyDB.Config()
        config.allowCreate  = true
        val ds              = BerkeleyDB.factory(folder, config)
        config.lockTimeout  = Duration(Prefs.dbLockTimeout.getOrElse(Prefs.defaultDbLockTimeout), TimeUnit.MILLISECONDS)
        val doc             = fun(folder, ds)
        ActionOpenWorkspace.openGUI(doc)
        Some(doc)

      } catch {
        case NonFatal(e) =>
          Dialog.showMessage(
            message     = s"Unable to create new document ${folder.path} \n\n${GUI.formatException(e)}",
            title       = fullTitle,
            messageType = Dialog.Message.Error
          )
          None
      }
    }
}
