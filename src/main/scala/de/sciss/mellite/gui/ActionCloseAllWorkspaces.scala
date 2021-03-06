/*
 *  ActionCloseAllWorkspaces.scala
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

import de.sciss.desktop
import de.sciss.desktop.KeyStrokes._
import de.sciss.desktop.Window
import de.sciss.lucre.stm.Sys
import de.sciss.lucre.swing.deferTx
import de.sciss.processor.Processor.Aborted
import de.sciss.synth.proc.Workspace

import scala.collection.breakOut
import scala.concurrent.{Future, Promise}
import scala.language.existentials
import scala.swing.Action
import scala.swing.event.Key
import scala.util.Success

object ActionCloseAllWorkspaces extends Action("Close All") {
  accelerator = Some(menu1 + shift + Key.W)

  private def dh = Application.documentHandler

  private def checkCloseAll(): Unit = enabled = dh.documents.nonEmpty

  checkCloseAll()

  dh.addListener {
    case desktop.DocumentHandler.Added  (_) => checkCloseAll()
    case desktop.DocumentHandler.Removed(_) => checkCloseAll()
  }

  def apply(): Unit = tryCloseAll()

  def tryCloseAll(): Future[Unit] = {
    val docs = dh.documents.toList  // iterator wil be exhausted!

    def loop(in: Future[Unit], rem: List[Workspace[_ <: Sys[_]]]): Future[Unit] = rem match {
      case Nil => in
      case head :: tail =>
        val headT = head.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }]
        in.value match {
          case Some(Success(())) =>
            val fut = tryClose(headT, None)
            loop(fut, tail)

          case _ =>
            in.flatMap { _ =>
              val fut = tryClose(headT, None)
              loop(fut, tail)
            }
        }
    }

    loop(Future.successful(()), docs)

//    // cf. http://stackoverflow.com/questions/20982681/existential-type-or-type-parameter-bound-failure
//    val allOk = docs.forall(doc => check(doc.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }], None))
//    if (allOk) docs.foreach(doc => close(doc.asInstanceOf[Workspace[~] forSome { type ~ <: Sys[~] }]))
  }

  private final class InMemoryVeto[S <: Sys[S]](workspace: Workspace[S], window: Option[Window])
    extends Veto[S#Tx] {

    def vetoMessage(implicit tx: S#Tx): String = "Closing an in-memory workspace."

    /** Attempts to resolve the veto condition by consulting the user.
      *
      * @return successful future if the situation is resolved, e.g. the user agrees to
      *         proceed with the operation. failed future if the veto is upheld, and
      *         the caller should abort the operation.
      */
    def tryResolveVeto()(implicit tx: S#Tx): Future[Unit] = {
      val p = Promise[Unit]()
      deferTx {
        val msg = s"<html><body>$vetoMessage That means<br>" +
          "all contents will be <b>irrevocably lost</b>.<br>" +
          "<p>Ok to proceed?</body></html>"
        val opt = desktop.OptionPane.confirmation(message = msg, messageType = desktop.OptionPane.Message.Warning,
          optionType = desktop.OptionPane.Options.OkCancel)
        import de.sciss.equal.Implicits._
        val ok = opt.show(window, "Close Workspace") === desktop.OptionPane.Result.Ok
        if (ok) p.success(()) else p.failure(Aborted())
      }
      p.future
    }
  }

  /** Checks if the workspace can be safely closed. This is the case
    * for durable and confluent workspaces. For an in-memory workspace,
    * the user is presented with a confirmation dialog.
    *
    * @param  workspace     the workspace to check
    * @param  window  a reference window if a dialog is presented
    *
    * @return `true` if it is ok to close the workspace, `false` if the request was denied
    */
  def prepareDisposal[S <: Sys[S]](workspace: Workspace[S], window: Option[Window])(implicit tx: S#Tx): Option[Veto[S#Tx]] = {
    val vetoInMemOpt: Option[Veto[S#Tx]] =
      workspace match {
        case _: Workspace.InMemory => Some(new InMemoryVeto[S](workspace, window))
        case _ => None
      }

    collectVetos(workspace, vetoInMemOpt)
  }

  def tryClose[S <: Sys[S]](workspace: Workspace[S], window: Option[Window]): Future[Unit] = {
    def succeed()(implicit tx: S#Tx): Future[Unit] = {
      workspace.dispose()
      Future.successful(())
    }

    def complete(): Unit =
      workspace.cursor.step { implicit tx =>
        workspace.dispose()
      }

    workspace.cursor.step { implicit tx =>
      val vetoOpt = prepareDisposal(workspace, window)
      vetoOpt.fold[Future[Unit]] {
        succeed()
      } { veto =>
        val futVeto = veto.tryResolveVeto()
        futVeto.value match {
          case Some(Success(()))  => succeed()
          case _                  => futVeto.map(_ => complete())
        }
      }
    }
  }

  private def collectVetos[S <: Sys[S]](workspace: Workspace[S], preOpt: Option[Veto[S#Tx]])
                                       (implicit tx: S#Tx): Option[Veto[S#Tx]] = {
    val list0: List[Veto[S#Tx]] = workspace.dependents.flatMap {
      case mv: DependentMayVeto[S#Tx] /* if mv != self */ => mv.prepareDisposal()
      case _ => None
    } (breakOut)

    val list = preOpt.fold(list0)(_ :: list0)

    list match {
      case Nil  => None
      case _    =>
        val res = new Veto[S#Tx] {
          def vetoMessage(implicit tx: S#Tx): String =
            list.map(_.vetoMessage).mkString("\n")

          def tryResolveVeto()(implicit tx: S#Tx): Future[Unit] = {
            def loop(in: Future[Unit], rem: List[Veto[S#Tx]]): Future[Unit] = rem match {
              case Nil => in
              case head :: tail =>
                in.value match {
                  case Some(Success(())) =>
                    val andThen = head.tryResolveVeto()
                    loop(andThen, tail)
                  case _ => in.flatMap { _ =>
                    workspace.cursor.step { implicit tx =>
                      val andThen = head.tryResolveVeto()
                      loop(andThen, tail)
                    }
                  }
                }
            }

            loop(Future.successful(()), list)
          }
        }
        Some(res)
    }
  }

//  /** Checks if the workspace may be closed (calling `check`) and if so, disposes the workspace.
//    *
//    * @param  doc     the workspace to check
//    * @param  window  a reference window if a dialog is presented
//    *
//    * @return `true` if the space was closed
//    */
//  def checkAndClose[S <: Sys[S]](doc: Workspace[S], window: Option[Window]): Boolean = ...
////    check(doc, window) && {
////      close(doc)
////      true
////    }

//  /** Closes the provided workspace without checking. */
//  def close[S <: Sys[S]](doc: Workspace[S]): Unit = {
//    requireEDT()
//    log(s"Closing workspace ${doc.folder}")
//    dh.removeDocument(doc)
//    // doc.close()
//  }
}