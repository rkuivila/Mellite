package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import de.sciss.model.impl.ModelImpl

final class ProcSelectionModelImpl[S <: Sys[S]]
  extends ProcSelectionModel[S] with ModelImpl[ProcSelectionModel.Update[S]] {

  import ProcSelectionModel.Update

  // no sync because we assume the model is only used on the event thread
  private var set = Set.empty[TimelineProcView[S]]

  def contains(view: TimelineProcView[S]): Boolean = set.contains(view)

  def +=(view: TimelineProcView[S]) {
    if (!set.contains(view)) {
      set += view
      dispatch(Update(added = Set(view), removed = Set.empty))
    }
  }

  def -=(view: TimelineProcView[S]) {
    if (set.contains(view)) {
      set -= view
      dispatch(Update(added = Set.empty, removed = Set(view)))
    }
  }

  def clear() {
    if (set.nonEmpty) {
      val removed = set
      set = Set.empty
      dispatch(Update(added = Set.empty, removed = removed))
    }
  }
}