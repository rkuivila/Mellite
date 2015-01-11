/*
 *  ScansView.scala
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

import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.swing.View
import de.sciss.lucre.event.Sys
import de.sciss.synth.proc.Proc
import impl.{ScansViewImpl => Impl}

object ScansView {
  final case class Drag[S <: Sys[S]](workspace: Workspace[S], proc: stm.Source[S#Tx, Proc.Obj[S]], key: String)

  final val flavor = DragAndDrop.internalFlavor[Drag[_]]

  def apply[S <: Sys[S]](obj: Proc.Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                           workspace: Workspace[S], undoManager: UndoManager): ScansView[S] =
    Impl(obj)
}
trait ScansView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S]