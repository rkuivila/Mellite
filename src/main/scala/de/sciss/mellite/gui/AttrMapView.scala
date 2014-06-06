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

package de.sciss.mellite.gui

import de.sciss.lucre.swing.View
import de.sciss.synth.proc.Obj
import de.sciss.lucre.stm
import impl.document.{AttrMapViewImpl => Impl}
import de.sciss.desktop.UndoManager
import de.sciss.lucre.synth.Sys

object AttrMapView {
  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S],
                                      undoManager: UndoManager): AttrMapView[S] =
    Impl(obj)

  type Selection[S <: Sys[S]] = List[(String, ObjView[S])]
}
trait AttrMapView[S <: Sys[S]] extends View.Editable[S] {
  def selection: AttrMapView.Selection[S]
}