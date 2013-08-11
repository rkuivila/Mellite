/*
 *  ProcSelectionModelImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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
import de.sciss.synth.proc.Sys

final class ProcSelectionModelImpl[S <: Sys[S]]
  extends ProcSelectionModel[S] with ModelImpl[ProcSelectionModel.Update[S]] {

  import ProcSelectionModel.Update

  // no sync because we assume the model is only used on the event thread
  private var set = Set.empty[TimelineProcView[S]]

  def contains(view: TimelineProcView[S]): Boolean = set.contains(view)

  def iterator: Iterator[TimelineProcView[S]] = set.iterator

  def +=(view: TimelineProcView[S]): Unit =
    if (!set.contains(view)) {
      set += view
      dispatch(Update(added = Set(view), removed = Set.empty))
    }

  def -=(view: TimelineProcView[S]): Unit =
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