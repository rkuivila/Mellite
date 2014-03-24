/*
 *  FolderView.scala
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

import swing.Component
import impl.document.{FolderViewImpl => Impl}
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.stm.{Cursor, Disposable}
import de.sciss.model.Model
import de.sciss.lucre.stm
import java.io.File
import de.sciss.lucre.synth.Sys

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