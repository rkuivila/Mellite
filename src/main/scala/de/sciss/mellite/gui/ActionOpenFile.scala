/*
 *  ActionOpenFile.scala
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
import de.sciss.desktop.{Menu, RecentFiles, FileDialog, KeyStrokes}
import util.control.NonFatal
import java.io.File
import de.sciss.lucre.synth.Sys

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

  def perform(folder: File): Unit =
    try {
      val doc = Document.read(folder)
      openGUI(doc)

    } catch {
      case NonFatal(e) =>
        Dialog.showMessage(
          message     = "Unabled to create new document " + folder.getPath + "\n\n" + formatException(e),
          title       = fullTitle,
          messageType = Dialog.Message.Error
        )
    }
}