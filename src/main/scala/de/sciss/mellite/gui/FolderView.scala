/*
 *  FolderView.scala
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
import impl.{FolderViewImpl => Impl}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.stm.{Cursor, Disposable}
import de.sciss.model.Model
import de.sciss.lucre.stm
import java.io.File

object FolderView {
  def apply[S <: Sys[S]](document: Document[S], root: Folder[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): FolderView[S] = Impl(document, root)

  /** A selection is a sequence of paths, where a path is a prefix of folders and a trailing element.
    * The prefix is guaranteed to be non-empty.
    */
  type Selection[S <: Sys[S]] = Vec[(Vec[ElementView.FolderLike[S]], ElementView[S])]

  final case class SelectionDnDData[S <: Sys[S]](document: Document[S], selection: Selection[S])

  // Document not serializable -- local JVM only DnD -- cf. stackoverflow #10484344
  val selectionFlavor = DragAndDrop.internalFlavor[SelectionDnDData[_]]

  sealed trait Update[S <: Sys[S]] { def view: FolderView[S] }
  final case class SelectionChanged[S <: Sys[S]](view: FolderView[S], selection: Selection[S])
    extends Update[S]
}
trait FolderView[S <: Sys[S]] extends Model[FolderView.Update[S]] with Disposable[S#Tx] {
  def component: Component
  def selection: FolderView.Selection[S]

  def locations: Vec[ElementView.ArtifactLocation[S]]

  def findLocation(f: File): Option[stm.Source[S#Tx, Element.ArtifactLocation[S]]]
}