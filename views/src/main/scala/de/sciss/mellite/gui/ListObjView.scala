/*
 *  ListObjView.scala
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

import javax.swing.undo.UndoableEdit

import de.sciss.lucre.stm.{Sys, Obj}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.mellite.gui.impl.ListObjViewImpl

import scala.language.higherKinds
import scala.swing.{Component, Label}

object ListObjView {
  final case class Drag[S <: Sys[S]](workspace: Workspace[S], view: ObjView[S])

  // Document not serializable -- local JVM only DnD -- cf. stackoverflow #10484344
  // val SelectionFlavor = DragAndDrop.internalFlavor[SelectionDnDData[_]]
  val Flavor = DragAndDrop.internalFlavor[Drag[_]]

  trait Factory extends ObjView.Factory {
    def mkListView[S <: SSys[S]](obj: E[S])(implicit tx: S#Tx): ListObjView[S]
  }

  def addFactory(f: Factory): Unit = ListObjViewImpl.addFactory(f)

  def factories: Iterable[Factory] = ListObjViewImpl.factories

  def apply[S <: SSys[S]](obj: Obj[S])(implicit tx: S#Tx): ListObjView[S] = ListObjViewImpl(obj)
}
trait ListObjView[S <: stm.Sys[S]] extends ObjView[S] {
  /** Passes in a received opaque update to ask whether the view should be repainted due to this update.
    * This is a transactional method. If the view wants to update its internal state, it should
    * do that using `deferTx` to perform mutable state changes on the EDT, and then return `true` to
    * trigger a refresh of the table row.
    */
  def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean

  /** The opaque view value passed into the renderer. */
  def value: Any

  /** Configures the value cell renderer. The simplest case would be
    * `label.text = value.toString`. In order to leave the cell blank, just return the label.
    * One can also set its icon.
    */
  def configureRenderer(label: Label): Component

  /** Whether the opaque value part of the view can be edited in-place (inside the table itself). */
  def isEditable: Boolean

  /** Given that the view is editable, this method is called when the editor gave notification about
    * the editing being done. It is then the duty of the view to issue a corresponding transactional
    * mutation, returned in an undoable edit. Views that do not support editing should just return `None`.
    */
  def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit]
}