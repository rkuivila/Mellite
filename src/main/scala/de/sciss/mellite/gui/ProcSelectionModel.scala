/*
 *  ProcSelectionModel.scala
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

package de.sciss
package mellite
package gui

import de.sciss.model.Model
import de.sciss.mellite.gui.impl.timeline.{ProcSelectionModelImpl, ProcView}
import de.sciss.lucre.synth.Sys

object ProcSelectionModel {
  def apply[S <: Sys[S]]: ProcSelectionModel[S] = new ProcSelectionModelImpl[S]

  type Listener[S <: Sys[S]] = Model.Listener[Update[S]]
  final case class Update[S <: Sys[S]](added: Set[ProcView[S]], removed: Set[ProcView[S]])
}
trait ProcSelectionModel[S <: Sys[S]] extends Model[ProcSelectionModel.Update[S]] {
  def contains(view: ProcView[S]): Boolean
  def +=(view: ProcView[S]): Unit
  def -=(view: ProcView[S]): Unit
  def clear(): Unit
  def iterator: Iterator[ProcView[S]]
}