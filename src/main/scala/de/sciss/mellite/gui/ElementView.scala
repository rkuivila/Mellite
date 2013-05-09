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
import javax.swing.{Icon, ImageIcon}
import de.sciss.synth.expr.ExprImplicits
import scala.util.Try

object ElementView {
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double}
  import mellite.{Folder => _Folder}

  private[gui] def apply[S <: Sys[S]](parent: FolderLike[S], element: Element[S])
                                     (implicit tx: S#Tx): ElementView[S] = {
    val name = element.name.value
    element match {
      case e: Element.Int[S] =>
        val value = e.entity.value
        new Int.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.Double[S] =>
        val value = e.entity.value
        new Double.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.String[S] =>
        val value = e.entity.value
        new String.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.Folder[S] =>
        val res = new Folder.Impl(parent, tx.newHandle(e), name)
        res.children = e.entity.iterator.map(apply(res, _)(tx)).toIndexedSeq
        res
      case e: Element.ProcGroup[S] =>
        new ProcGroup.Impl(parent, tx.newHandle(e), name)
      case e: Element.AudioGrapheme[S] =>
        val value = e.entity.value
        new AudioGrapheme.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.ArtifactLocation[S] =>
        val value = e.entity.directory
        new ArtifactLocation.Impl(parent, tx.newHandle(e), name, value)
    }
  }

  // -------- String --------

  object String {
    private val icon = new ImageIcon(
      Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource("icon_string16.png"))
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

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.String[S]],
                                                       var name: _String, var value: _String)
      extends String[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key  .getTreeCellRendererComponent(tree.peer, name,  info.isSelected, false, true, info.row, info.hasFocus)
        Comp.value.getTreeCellRendererComponent(tree.peer, value, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "String"
      def icon = String.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = value match {
        case s: _String =>
          val imp = ExprImplicits[S]
          import imp._
          element().entity() = s
          true
        case _ => false
      }
    }
  }
  sealed trait String[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.String[S]]
  }

  // -------- Int --------

  object Int {
    private val icon = new ImageIcon(
      Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource("icon_integer16.png"))
    )

    // XXX TODO: DRY
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

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Int[S]],
                                                       var name: _String, var value: _Int)
      extends Int[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key  .getTreeCellRendererComponent(tree.peer, name,  info.isSelected, false, true, info.row, info.hasFocus)
        Comp.value.getTreeCellRendererComponent(tree.peer, value, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "Int"
      def icon = Int.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = {
        val numOpt = value match {
          case num: _Int  => Some(num)
          case s: _String => Try(s.toInt).toOption
        }
        numOpt.map { num =>
          val expr    = element().entity
          val changed = expr.value != num
          if (changed) {
            val imp = ExprImplicits[S]
            import imp._
            expr() = num
          }
          changed
        } .getOrElse(false)
      }
    }
  }
  sealed trait Int[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Int[S]]
  }

  // -------- Double --------

  object Double {
    private val icon = new ImageIcon(
      Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource("icon_float16.png"))
    )

    // XXX TODO: DRY
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

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Double[S]],
                                                       var name: _String, var value: _Double)
      extends Double[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key  .getTreeCellRendererComponent(tree.peer, name,  info.isSelected, false, true, info.row, info.hasFocus)
        Comp.value.getTreeCellRendererComponent(tree.peer, value, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "Double"
      def icon = Double.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = {
        val numOpt = value match {
          case num: _Double => Some(num)
          case s: _String => Try(s.toDouble).toOption
        }
        numOpt.map { num =>
          val expr = element().entity
          val changed = expr.value != num
          if (changed) {
            val imp = ExprImplicits[S]
            import imp._
            expr() = num
          }
          changed
        } .getOrElse(false)
      }
    }
  }
  sealed trait Double[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Double[S]]
  }

  // -------- FolderLike --------

  //  sealed trait BranchLike[S <: Sys[S]] extends Renderer[S] {
  //    def branchID(implicit tx: S#Tx): S#ID
  //
  //    def react(fun: S#Tx => _Folder.Update[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx]
  //
  //    /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
  //    var children: IIdxSeq[ElementView[S]]
  //
  //    def value {}
  //  }

  // sealed trait Branch[S <: Sys[S]] extends BranchLike[S] with ElementView[S]

  sealed trait FolderLike[S <: Sys[S]] extends Renderer[S] {
    def branchID(implicit tx: S#Tx) = folder.id

    def folder(implicit tx: S#Tx): _Folder[S]

    def react(fun: S#Tx => _Folder.Update[S] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      folder.changed.react { implicit tx => upd => {
        val fu = upd.changes.map {
          case LinkedList.Added  (idx, elem)  => _Folder.Added  (idx, elem)
          case LinkedList.Removed(idx, elem)  => _Folder.Removed(idx, elem)
          case LinkedList.Element(elem, eu)   => _Folder.Element(elem, eu)
        }
        fun(tx)(fu)
      }}

    /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
    var children: IIdxSeq[ElementView[S]]

    def value {}
  }

  // -------- Group --------

  object Folder {
    private final val cmpGroupJ = new DefaultTreeCellRenderer
    private final val cmpGroup  = Component.wrap(cmpGroupJ)

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Folder[S]],
                                                       var name: _String)
      extends Folder[S] with ElementView.Impl[S] {

      var children = IIdxSeq.empty[ElementView[S]]

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        // never show the leaf icon, always a folder icon. for empty folders, show the icon as if the folder is open
        cmpGroupJ.getTreeCellRendererComponent(tree.peer, name, info.isSelected, info.isExpanded || info.isLeaf,
          false /* info.isLeaf */, info.row, info.hasFocus)
        cmpGroup
      }

      def folder(implicit tx: S#Tx): _Folder[S] = element().entity

      def prefix = "Group"
      def icon = Swing.EmptyIcon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = false
    }
  }
  sealed trait Folder[S <: Sys[S]] extends FolderLike[S] with ElementView[S] /* Branch[S] */ {
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

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.ProcGroup[S]],
                                                       var name: _String)
      extends ProcGroup[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        cmpLabelJ.getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
        cmpLabel
      }
      def prefix = "ProcGroup"
      def value {}
      def icon = ProcGroup.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = false
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

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.AudioGrapheme[S]],
                                                       var name: _String, var value: Grapheme.Value.Audio)
      extends AudioGrapheme[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key  .getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
        val spec = value.spec.toString
        Comp.value.getTreeCellRendererComponent(tree.peer, spec, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "String"
      def icon = AudioGrapheme.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = false
    }
  }
  sealed trait AudioGrapheme[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.AudioGrapheme[S]]
    var value: Grapheme.Value.Audio
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

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.ArtifactLocation[S]],
                                                       var name: _String, var directory: File)
      extends ArtifactLocation[S] with ElementView.Impl[S] {

      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = {
        Comp.key.getTreeCellRendererComponent(tree.peer, name, info.isSelected, false, true, info.row, info.hasFocus)
        val value = directory.toString
        Comp.value.getTreeCellRendererComponent(tree.peer, value, info.isSelected, false, true, info.row, info.hasFocus)
        Comp
      }
      def prefix = "ArtifactStore"
      def value = directory
      def icon = ArtifactLocation.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx) = false
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
      import _Folder.serializer
      val res = new Impl(tx.newHandle(folder))
      res.children = folder.iterator.map(ElementView(res, _)(tx)).toIndexedSeq
      res
    }

    private final class Impl[S <: Sys[S]](handle: stm.Source[S#Tx, _Folder[S]])
      extends Root[S] {

      var children = IIdxSeq.empty[ElementView[S]]
      def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component = cmpBlank
      def folder(implicit tx: S#Tx): _Folder[S] = handle()
      def name = "root"
      def parent: Option[FolderLike[S]] = None
      def icon = Swing.EmptyIcon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = false
    }
  }
  sealed trait Root[S <: Sys[S]] extends FolderLike[S]

  private sealed trait Impl[S <: Sys[S]] extends ElementView[S] {
    protected def prefix: _String
    override def toString = s"ElementView.$prefix(name = $name)"
    protected def _parent: FolderLike[S]

    def parent: Option[FolderLike[S]] = Some(_parent)
  }

  sealed trait Renderer[S <: Sys[S]] {
    private[gui] def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
    def name: _String
    def parent: Option[ElementView.FolderLike[S]]
    def value: Any
    def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean
    def icon: Icon
  }
}
sealed trait ElementView[S <: Sys[S]] extends ElementView.Renderer[S] {
  def element: stm.Source[S#Tx, Element[S]]
  // def name: String
  // def parent: Option[ElementView.FolderLike[S]]
}