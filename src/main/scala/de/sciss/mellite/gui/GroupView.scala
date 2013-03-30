/*
 *  GroupView.scala
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

import de.sciss.synth.proc.Sys
import swing.Component
import impl.{GroupViewImpl => Impl}
import de.sciss.desktop.Model
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.stm.Disposable

object GroupView {
  def apply[S <: Sys[S]](root: Elements[S])(implicit tx: S#Tx): GroupView[S] = Impl(root)

  /** A selection is a sequence of paths, where a path is a prefix of groups and a trailing element.
    * The prefix is guaranteed to be non-empty.
    */
  type Selection[S <: Sys[S]] = IIdxSeq[(IIdxSeq[ElementView.GroupLike[S]], ElementView[S])]

  sealed trait Update[S <: Sys[S]] { def view: GroupView[S] }
  final case class SelectionChanged[S <: Sys[S]](view: GroupView[S], selection: Selection[S])
    extends Update[S]
}
trait GroupView[S <: Sys[S]] extends Model[GroupView.Update[S]] with Disposable[S#Tx] {
  def component: Component
  def selection: GroupView.Selection[S]
}