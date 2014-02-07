/*
 *  ActionOpenFile.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import swing.{Dialog, Action}
import java.awt.event.KeyEvent
import de.sciss.desktop.{Menu, RecentFiles, FileDialog, KeyStrokes}
import util.control.NonFatal
import java.io.File
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.{Mellite => App}
import language.existentials

object ActionOpenFile extends Action( "Open...") {
  import KeyStrokes._

  private val _recent = RecentFiles(Mellite.userPrefs("recent-docs")) { folder =>
    perform(folder)
  }

  accelerator = Some(menu1 + KeyEvent.VK_O)

  private def fullTitle = "Open Document"

  // XXX TODO: should be in another place
  def openGUI[S <: Sys[S]](doc: Document[S]): Unit = {
    recentFiles.add(doc.folder)
    Mellite.documentHandler.addDocument(doc)
    doc match {
      case cf: ConfluentDocument =>
        (cf: ConfluentDocument).system.durable.step { implicit tx =>
          DocumentCursorsFrame(cf)
        }
      case eph: EphemeralDocument =>
        implicit val csr = eph.cursor
        csr.step { implicit tx =>
          DocumentElementsFrame(eph)
        }
    }
  }

  def recentFiles: RecentFiles  = _recent
  def recentMenu : Menu.Group   = _recent.menu

  def apply(): Unit = {
    val dlg = FileDialog.open(title = fullTitle)
    dlg.setFilter { f => f.isDirectory && f.getName.endsWith(".mllt") }
    dlg.show(None).foreach(perform)
  }

  private def openView[S <: Sys[S]](doc: Document[S]): Unit =
    DocumentViewHandler.instance(doc).collectFirst {
      case dcv: DocumentCursorsView => dcv.window
    } .foreach(_.front())

  def perform(folder: File): Unit =
    App.documentHandler.documents.find(_.folder == folder).fold(doOpen(folder)) { doc =>

      val doc1 = doc.asInstanceOf[Document[S] forSome { type S <: Sys[S] }]
      openView(doc1)
    }

  private def doOpen(folder: File): Unit =
    try {
      val doc = Document.read(folder)
      openGUI(doc)

    } catch {
      case NonFatal(e) =>
        Dialog.showMessage(
          message     = "Unable to create new document " + folder.getPath + "\n\n" + formatException(e),
          title       = fullTitle,
          messageType = Dialog.Message.Error
        )
    }
}