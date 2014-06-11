/*
 *  AttrMapView.scala
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

import de.sciss.lucre.swing.View
import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm
import impl.document.{AttrMapViewImpl => Impl}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.synth.Sys
import de.sciss.model.Model

object AttrMapView {
  def apply[S <: Sys[S]](workspace: Workspace[S], obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                         undoManager: UndoManager): AttrMapView[S] =
    Impl(workspace, obj)

  type Selection[S <: Sys[S]] = List[(String, ObjView[S])]

  sealed trait Update[S <: Sys[S]] { def view: AttrMapView[S] }
  final case class SelectionChanged[S <: Sys[S]](view: AttrMapView[S], selection: Selection[S])
    extends Update[S]
}
trait AttrMapView[S <: Sys[S]] extends ViewHasWorkspace[S] with View.Editable[S] with Model[AttrMapView.Update[S]] {
  def selection: AttrMapView.Selection[S]
}