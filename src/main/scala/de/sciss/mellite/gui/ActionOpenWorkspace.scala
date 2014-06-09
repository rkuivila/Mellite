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
import de.sciss.desktop.{Desktop, Menu, RecentFiles, FileDialog, KeyStrokes}
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
        (cf: Workspace.Confluent).system.durable.step { implicit tx =>
          DocumentCursorsFrame(cf)
        }
      case eph: Workspace.Ephemeral =>
        implicit val csr = eph.cursor
        csr.step { implicit tx =>
          DocumentElementsFrame[proc.Durable, proc.Durable](eph, None)
        }
    }
  }

  def recentFiles: RecentFiles  = _recent
  def recentMenu : Menu.Group   = _recent.menu

  def apply(): Unit = {
    val dlg = if (Desktop.isMac) {
      val res = FileDialog.open(title = fullTitle)
      res.setFilter { f => f.isDirectory && f.ext.toLowerCase == Workspace.ext }
      res
    } else {
      val res = FileDialog.open(title = fullTitle)
      // "Filename filters do not function in Sun's reference implementation for Microsoft Windows"
      // ... and Linux neither. Suckers.
      //      res.setFilter { f =>
      //        val res = f.name.toLowerCase == "open" && f.parentOption.exists(_.ext.toLowerCase == Workspace.ext)
      //        println(s"TEST '${f.path}' = $res")
      //        res
      //      }
      res
    }
    dlg.show(None).foreach { f =>
      val f1 = if (Desktop.isMac) f else f.parent
      perform(f1)
    }
  }

  private def openView[S <: Sys[S]](doc: Workspace[S]): Unit =
    DocumentViewHandler.instance(doc).collectFirst {
      case dcv: DocumentCursorsView => dcv.window
    } .foreach(_.front())

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
          message     = s"Unable to create new workspace ${folder.path}\n\n${formatException(e)}",
          title       = fullTitle,
          messageType = Dialog.Message.Error
        )
    }
}