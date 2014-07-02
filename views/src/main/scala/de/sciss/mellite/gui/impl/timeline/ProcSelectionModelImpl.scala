/*
 *  ProcSelectionModelImpl.scala
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
package impl
package timeline

import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.synth.Sys

final class ProcSelectionModelImpl[S <: Sys[S]]
  extends ProcSelectionModel[S] with ModelImpl[ProcSelectionModel.Update[S]] {

  import ProcSelectionModel.Update

  // no sync because we assume the model is only used on the event thread
  private var set = Set.empty[ProcView[S]]

  def contains(view: ProcView[S]): Boolean = set.contains(view)

  def iterator: Iterator[ProcView[S]] = set.iterator

  def +=(view: ProcView[S]): Unit =
    if (!set.contains(view)) {
      set += view
      dispatch(Update(added = Set(view), removed = Set.empty))
    }

  def -=(view: ProcView[S]): Unit =
    if (set.contains(view)) {
      set -= view
      dispatch(Update(added = Set.empty, removed = Set(view)))
    }

  def clear(): Unit =
    if (set.nonEmpty) {
      val removed = set
      set = Set.empty
      dispatch(Update(added = Set.empty, removed = removed))
    }
}