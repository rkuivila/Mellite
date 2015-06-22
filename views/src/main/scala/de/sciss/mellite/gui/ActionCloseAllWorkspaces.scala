/*
 *  ActionCloseAllWorkspaces.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import de.sciss.desktop
import de.sciss.desktop.KeyStrokes._
import de.sciss.desktop.Window
import de.sciss.lucre.event.Sys
import de.sciss.lucre.swing.requireEDT

import scala.language.existentials
import scala.swing.Action
import scala.swing.event.Key

object ActionCloseAllWorkspaces extends Action("Close All") {
  accelerator = Some(menu1 + shift + Key.W)

  private def dh = Application.documentHandler

  private def checkCloseAll(): Unit = enabled = dh.documents.nonEmpty

  checkCloseAll()

  dh.addListener {
    case desktop.DocumentHandler.Added(doc) =>
      checkCloseAll()

    case desktop.DocumentHandler.Removed(doc) =>
      checkCloseAll()
  }

  def apply(): Unit = {
    val docs = dh.documents.toList  // iterator wil be exhausted!

    // cf. http://stackoverflow.com/questions/20982681/existential-type-or-type-parameter-bound-failure
    val allOk = docs.forall(doc => check(doc.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }], None))
    if (allOk) docs.foreach(doc => close(doc.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }]))
  }

  /** Checks if the workspace can be safely closed. This is the case
    * for durable and confluent workspaces. For an in-memory workspace,
    * the user is presented with a confirmation dialog.
    *
    * @param  doc     the workspace to check
    * @param  window  a reference window if a dialog is presented
    *
    * @return `true` if it is ok to close the workspace, `false` if the request was denied
    */
  def check[S <: Sys[S]](doc: Workspace[S], window: Option[Window]): Boolean = {
    requireEDT()
    doc match {
      case docI: Workspace.InMemory =>
        val msg = "<html><body>Closing an in-memory workspace means<br>" +
          "all contents will be <b>irrevocably lost</b>.<br>" +
          "<p>Ok to proceed?</body></html>"
        val opt = desktop.OptionPane.confirmation(message = msg, messageType = desktop.OptionPane.Message.Warning,
          optionType = desktop.OptionPane.Options.OkCancel)
        opt.show(window, "Close Workspace") == desktop.OptionPane.Result.Ok

      case _=> true
    }
  }

  /** Checks if the workspace may be closed (calling `check`) and if so, disposes the workspace.
    *
    * @param  doc     the workspace to check
    * @param  window  a reference window if a dialog is presented
    *
    * @return `true` if the space was closed
    */
  def checkAndClose[S <: Sys[S]](doc: Workspace[S], window: Option[Window]): Boolean =
    check(doc, window) && {
      close(doc)
      true
    }

  /** Closes the provided workspace without checking. */
  def close[S <: Sys[S]](doc: Workspace[S]): Unit = {
    requireEDT()
    log(s"Closing workspace ${doc.folder}")
    dh.removeDocument(doc)
    // doc.close()
  }

//    doc.cursor.step { implicit tx =>
//      doc.dispose()
//    }
}