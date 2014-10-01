/*
 *  ActionOpenWorkspace.scala
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
import de.sciss.desktop.{Menu, RecentFiles, FileDialog, KeyStrokes}
import util.control.NonFatal
import de.sciss.file._
import de.sciss.lucre.synth.Sys
import language.existentials
import scala.swing.event.Key
import de.sciss.synth.proc

object ActionOpenWorkspace extends Action("Open...") {
  import KeyStrokes._

  private val _recent = RecentFiles(Application.userPrefs("recent-docs")) { folder =>
    perform(folder)
  }

  accelerator = Some(menu1 + Key.O)

  private def fullTitle = "Open Workspace"

  // XXX TODO: should be in another place
  def openGUI[S <: Sys[S]](doc: Workspace[S]): Unit = {
    recentFiles.add(doc.folder)
    Application.documentHandler.addDocument(doc)
    doc match {
      case cf: Workspace.Confluent =>
        implicit val workspace = cf
        (cf: Workspace.Confluent).system.durable.step { implicit tx =>
          DocumentCursorsFrame(cf)
        }
      case eph: Workspace.Ephemeral =>
        implicit val csr        = eph.cursor
        implicit val workspace  = eph
        val nameView = ExprView.const[proc.Durable, Option[String]](None)
        csr.step { implicit tx =>
          FolderFrame[proc.Durable, proc.Durable](name = nameView,
            isWorkspaceRoot = true)
        }
    }
  }

  def recentFiles: RecentFiles  = _recent
  def recentMenu : Menu.Group   = _recent.menu

  // private def isWebLaF = javax.swing.UIManager.getLookAndFeel.getName == "WebLookAndFeel"

  def apply(): Unit = {
    val dlg = FileDialog.open(title = fullTitle)
    // if (!isWebLaF) {
      // filter currently doesn't work with WebLaF
      dlg.setFilter { f => f.isDirectory && f.ext.toLowerCase == Workspace.ext}
    // }
    dlg.show(None).foreach(perform)
  }

  private def openView[S <: Sys[S]](doc: Workspace[S]): Unit = ()
// MMM
//    DocumentViewHandler.instance(doc).collectFirst {
//      case dcv: DocumentCursorsView => dcv.window
//    } .foreach(_.front())

  def perform(folder: File): Unit =
    Application.documentHandler.documents.find(_.folder == folder).fold(doOpen(folder)) { doc =>

      val doc1 = doc.asInstanceOf[Workspace[S] forSome { type S <: Sys[S] }]
      openView(doc1)
    }

  private def doOpen(folder: File): Unit =
    try {
      val doc = Workspace /* .Confluent */ .read(folder)
      doc match {
        case cf : Workspace.Confluent => openGUI(cf )
        case dur: Workspace.Ephemeral => openGUI(dur)
      }
      // openGUI(doc)

    } catch {
      case NonFatal(e) =>
        Dialog.showMessage(
          message     = s"Unable to create new workspace ${folder.path}\n\n${GUI.formatException(e)}",
          title       = fullTitle,
          messageType = Dialog.Message.Error
        )
    }
}