/*
 *  CodeView.scala
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

import javax.swing.undo.UndoableEdit

import de.sciss.lucre.event.Sys
import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.synth.proc.Obj
import impl.interpreter.{CodeViewImpl => Impl}
import de.sciss.model.Model
import scala.swing.Action
import scala.concurrent.Future

object CodeView {
  trait Handler[S <: Sys[S], +In, -Out] extends Disposable[S#Tx] {
    def in: In
    def save(out: Out)(implicit tx: S#Tx): UndoableEdit
  }

  /** If `graph` is given, the `apply` action is tied to updating the graph variable. */
  def apply[S <: Sys[S]](obj: Obj.T[S, Code.Elem], code0: Code)
                        (handler: Option[Handler[S, code0.In, code0.Out]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): CodeView[S] =
    Impl(obj, code0)(handler)

  sealed trait Update
  case class DirtyChange(value: Boolean) extends Update
}
trait CodeView[S <: Sys[S]] extends ViewHasWorkspace[S] with Model[CodeView.Update] {
  def isCompiling: Boolean

  def dirty: Boolean

  def save(): Future[Unit]

  // def updateSource(text: String)(implicit tx: S#Tx): Unit

  def undoAction: Action
  def redoAction: Action
}