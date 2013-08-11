package de.sciss
package mellite
package gui

import de.sciss.synth.proc.Sys
import de.sciss.model.Model
import de.sciss.mellite.gui.impl.timeline.{ProcSelectionModelImpl, TimelineProcView}

object ProcSelectionModel {
  def apply[S <: Sys[S]]: ProcSelectionModel[S] = new ProcSelectionModelImpl[S]

  type Listener[S <: Sys[S]] = Model.Listener[Update[S]]
  final case class Update[S <: Sys[S]](added: Set[TimelineProcView[S]], removed: Set[TimelineProcView[S]])
}
trait ProcSelectionModel[S <: Sys[S]] extends Model[ProcSelectionModel.Update[S]] {
  def contains(view: TimelineProcView[S]): Boolean
  def +=(view: TimelineProcView[S]): Unit
  def -=(view: TimelineProcView[S]): Unit
  def clear(): Unit
  def iterator: Iterator[TimelineProcView[S]]
}