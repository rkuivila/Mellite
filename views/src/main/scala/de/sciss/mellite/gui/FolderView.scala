/*
 *  FolderView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import java.io.File

import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.{TreeTableView, View}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.ArtifactLocationObjView
import de.sciss.mellite.gui.impl.document.{FolderViewImpl => Impl}
import de.sciss.model.Model
import de.sciss.synth.proc.Folder

import scala.collection.breakOut
import scala.collection.immutable.{IndexedSeq => Vec}

object FolderView {
  def apply[S <: Sys[S]](root: Folder[S])
                         (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                          undoManager: UndoManager): FolderView[S] = Impl(root)

  /** A selection is a sequence of paths, where a path is a prefix of folders and a trailing element.
    * The prefix is guaranteed to be non-empty.
    */
  // type Selection[S <: Sys[S]] = Vec[(Vec[ObjView.FolderLike[S]], ObjView[S])]
  type Selection[S <: Sys[S]] = List[TreeTableView.NodeView[S, Obj[S], Folder[S], ListObjView[S]]]
  // type Selection[S <: Sys[S]] = Vec[stm.Source[S#Tx, Obj[S]]]

  final case class SelectionDnDData[S <: Sys[S]](workspace: Workspace[S], selection: Selection[S]) {
    lazy val types: Set[Int] = selection.map(_.renderData.factory.tpe.typeID)(breakOut)
  }

  // Document not serializable -- local JVM only DnD -- cf. stackoverflow #10484344
  val SelectionFlavor = DragAndDrop.internalFlavor[SelectionDnDData[_]]

  sealed trait Update[S <: Sys[S]] { def view: FolderView[S] }
  final case class SelectionChanged[S <: Sys[S]](view: FolderView[S], selection: Selection[S])
    extends Update[S]
}
trait FolderView[S <: Sys[S]] extends Model[FolderView.Update[S]] with View.Editable[S] {
  def selection: FolderView.Selection[S]

  def locations: Vec[ArtifactLocationObjView[S]]

  def insertionPoint(implicit tx: S#Tx): (Folder[S], Int)

  def findLocation(f: File): Option[ActionArtifactLocation.QueryResult[S]]

  def root: stm.Source[S#Tx, Folder[S]]
}