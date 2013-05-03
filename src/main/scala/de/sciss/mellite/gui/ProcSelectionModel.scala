package de.sciss
package mellite
package gui

import mellite.gui.impl.{ProcSelectionModelImpl, TimelineProcView}
import de.sciss.synth.proc.Sys
import de.sciss.model.Model

object ProcSelectionModel {
  def apply[S <: Sys[S]]: ProcSelectionModel[S] = new ProcSelectionModelImpl[S]

  final case class Update[S <: Sys[S]](added: Set[TimelineProcView[S]], removed: Set[TimelineProcView[S]])
}
trait ProcSelectionModel[S <: Sys[S]] extends Model[ProcSelectionModel.Update[S]] {
  def contains(view: TimelineProcView[S]): Boolean
  def +=(view: TimelineProcView[S]): Unit
  def -=(view: TimelineProcView[S]): Unit
  def clear(): Unit
}