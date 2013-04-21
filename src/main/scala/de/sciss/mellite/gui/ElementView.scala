/*
 *  ElementView.scala
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

package de.sciss
package mellite
package gui

import synth.proc.{Grapheme, Sys}
import lucre.stm
import scalaswingcontrib.tree.Tree
import swing.{Label, Swing, BoxPanel, Orientation, Component}
import collection.immutable.{IndexedSeq => IIdxSeq}
import javax.swing.tree.DefaultTreeCellRenderer
import Swing._
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.expr.LinkedList
import java.io.File
import java.awt.Toolkit
import javax.swing.ImageIcon

object ElementView {
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double}
  import mellite.{Folder => _Folder}

  private[gui] def apply[S <: Sys[S]](element: Element[S])(implicit tx: S#Tx): ElementView[S] = {
    val name = element.name.value
    element match {
      case e: Element.Int[S] =>
        val value = e.entity.value
        new Int.Impl(tx.newHandle(e), name, value)
      case e: Element.Double[S] =>
        val value = e.entity.value
        new Double.Impl(tx.newHandle(e), name, value)
      case e: Element.String[S] =>
        val value = e.entity.value
        new String.Impl(tx.newHandle(e), name, value)
      case e: Element.Folder[S] =>
        val children = e.entity.iterator.map(apply(_)(tx)).toIndexedSeq
        new Folder.Impl(tx.newHandle(e), name, children)
      case e: Element.ProcGroup[S] =>
        new ProcGroup.Impl(tx.newHandle(e), name)
      case e: Element.AudioGrapheme[S] =>
        val value = e.entity.value
        new AudioGrapheme.Impl(tx.newHandle(e), name, value)
      case e: Element.ArtifactLocation[S] =>
        val value = e.entity.directory
        new ArtifactLocation.Impl(tx.newHandle(e), name, value)
    }
  }

  // -------- String --------

  object String {
    private object Comp extends BoxPanel(Orientation.Horizontal) {
      val key = new DefaultTreeCellRenderer
      key.setLeafIcon(null)
      val value = new DefaultTreeCellRenderer
      value.setLeafIcon(null)
      background = null
      contents += Component.wrap(key)
      contents += HStrut(8)
      contents += Component.wrap(value)
    }

    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.String[S]],
                                                       var name: _String, var value: _String)
      extends String[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key  .getTreeCellRendererComponent(tree.peer, name,  info.isSelected, false, true, info.row, info.hasFocus)
        Comp.value.getTreeCellRendererComponent(tree.peer, value, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "String"
    }
  }
  sealed trait String[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.String[S]]
  }

  // -------- Int --------

  object Int {
    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.Int[S]],
                                                      var name: _String, var value: _Int)
      extends Int[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = ??? // cmpBlank // XXX TODO
      def prefix = "Int"
    }
  }
  sealed trait Int[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Int[S]]
  }

  // -------- Double --------

  object Double {
    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.Double[S]],
                                                       var name: _String, var value: _Double)
      extends Double[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = ??? // cmpBlank // XXX TODO
      def prefix = "Double"
    }
  }
  sealed trait Double[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Double[S]]
  }

  // -------- FolderLike --------

  sealed trait BranchLike[S <: Sys[S]] extends Renderer {
    def branchID(implicit tx: S#Tx): S#ID

    def reactTx(fun: S#Tx => _Folder.Update[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx]

    /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
    var children: IIdxSeq[ElementView[S]]
  }

  sealed trait Branch[S <: Sys[S]] extends BranchLike[S] with ElementView[S]

  sealed trait FolderLike[S <: Sys[S]] extends BranchLike[S] {
    def branchID(implicit tx: S#Tx) = folder.id

    def folder(implicit tx: S#Tx): _Folder[S]

    def reactTx(fun: S#Tx => _Folder.Update[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      folder.changed.reactTx[LinkedList.Update[S, Element[S], Element.Update[S]]] { implicit tx => upd => {
        val fu = upd.changes.map {
          case LinkedList.Added  (idx, elem)  => _Folder.Added  (idx, elem)
          case LinkedList.Removed(idx, elem)  => _Folder.Removed(idx, elem)
          case LinkedList.Element(elem, eu)   => _Folder.Element(elem, eu)
        }
        fun(tx)(fu)
      }}
  }

  // -------- Group --------

  object Folder {
    private final val cmpGroupJ = new DefaultTreeCellRenderer
    private final val cmpGroup  = Component.wrap(cmpGroupJ)

    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.Folder[S]],
                                                       var name: _String, var children: IIdxSeq[ElementView[S]])
      extends Folder[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        // never show the leaf icon, always a folder icon. for empty folders, show the icon as if the folder is open
        cmpGroupJ.getTreeCellRendererComponent(tree.peer, name, info.isSelected, info.isExpanded || info.isLeaf,
          false /* info.isLeaf */, info.row, info.hasFocus)
        cmpGroup
      }

      def folder(implicit tx: S#Tx): _Folder[S] = element().entity

      def prefix = "Group"
    }
  }
  sealed trait Folder[S <: Sys[S]] extends FolderLike[S] with Branch[S] {
    def element: stm.Source[S#Tx, Element.Folder[S]]
  }

  // -------- ProcGroup --------

  object ProcGroup {
    private val icon = new ImageIcon(
      Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource("icon_procgroup16.png"))
    )

    private final val cmpLabelJ = new DefaultTreeCellRenderer
    cmpLabelJ.setLeafIcon(icon)
    private final val cmpLabel  = Component.wrap(cmpLabelJ)

    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.ProcGroup[S]],
                                                       var name: _String)
      extends ProcGroup[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpLabelJ.getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
        cmpLabel
      }
      def prefix = "ProcGroup"
    }
  }
  sealed trait ProcGroup[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.ProcGroup[S]]
  }

  // -------- AudioGrapheme --------

  object AudioGrapheme {
    private val icon = new ImageIcon(
      Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource("icon_audiographeme16.png"))
    )

    private object Comp extends BoxPanel(Orientation.Horizontal) {
      val key = new DefaultTreeCellRenderer
      key.setLeafIcon(icon)
      val value = new DefaultTreeCellRenderer
      value.setLeafIcon(null)
      background = null
      contents += Component.wrap(key)
      contents += HStrut(8)
      contents += Component.wrap(value)
    }

    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.AudioGrapheme[S]],
                                                       var name: _String, var value: Grapheme.Value.Audio)
      extends AudioGrapheme[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key  .getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
        val spec = value.spec.toString
        Comp.value.getTreeCellRendererComponent(tree.peer, spec, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "String"
    }
  }
  sealed trait AudioGrapheme[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.AudioGrapheme[S]]
  }

  // -------- ArtifactStore --------

  object ArtifactLocation {
    private val icon = new ImageIcon(
      Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource("icon_location16.png"))
    )

    private object Comp extends BoxPanel(Orientation.Horizontal) {
      val key = new DefaultTreeCellRenderer
      key.setLeafIcon(icon)
      val value = new DefaultTreeCellRenderer
      value.setLeafIcon(null)
      background = null
      contents += Component.wrap(key)
      contents += HStrut(8)
      contents += Component.wrap(value)
    }

    private[ElementView] final class Impl[S <: Sys[S]](val element: stm.Source[S#Tx, Element.ArtifactLocation[S]],
                                                       var name: _String, var directory: File)
      extends ArtifactLocation[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key.getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
        val spec = directory.toString
        Comp.value.getTreeCellRendererComponent(tree.peer, spec, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "ArtifactStore"
    }
  }
  sealed trait ArtifactLocation[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.ArtifactLocation[S]]
    var directory: File
  }

  // -------- Root --------

  object Root {
    private final val cmpBlank = new Label

    private[gui] def apply[S <: Sys[S]](folder: _Folder[S])(implicit tx: S#Tx): Root[S] = {
      val children = folder.iterator.map(ElementView(_)(tx)).toIndexedSeq
      import _Folder.serializer
      new Impl(tx.newHandle(folder), children)
    }

    private final class Impl[S <: Sys[S]](handle: stm.Source[S#Tx, _Folder[S]],
                                          var children: IIdxSeq[ElementView[S]])
      extends Root[S] {
      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank
      def folder(implicit tx: S#Tx): _Folder[S] = handle()
    }
  }
  sealed trait Root[S <: Sys[S]] extends FolderLike[S]

  private sealed trait Impl[S <: Sys[S]] extends ElementView[S] {
    protected def prefix: _String
    override def toString = s"ElementView.$prefix(name = $name)"
  }

  sealed trait Renderer {
    private[gui] def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
  }
}
sealed trait ElementView[S <: Sys[S]] extends ElementView.Renderer {
  def element: stm.Source[S#Tx, Element[S]]
  def name: String
}