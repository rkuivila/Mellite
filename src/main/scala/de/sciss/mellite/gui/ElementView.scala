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

import de.sciss.synth.proc.{Artifact, Grapheme, Sys}
import lucre.stm
import swing.Swing
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.expr.LinkedList
import java.io.File
import java.awt.Toolkit
import javax.swing.{Icon, ImageIcon}
import de.sciss.synth.expr.ExprImplicits
import scala.util.Try
import de.sciss.lucre.event.Change

object ElementView {
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double}
  import mellite.{Folder => _Folder} // , Recursion => _Recursion, Code => _Code}

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
        // res.children = e.entity.iterator.map(apply(res, _)(tx)).toIndexedSeq
        res
      case e: Element.ProcGroup[S] =>
        new ProcGroup.Impl(parent, tx.newHandle(e), name)
      case e: Element.AudioGrapheme[S] =>
        val value = e.entity.value
        new AudioGrapheme.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.ArtifactLocation[S] =>
        val value = e.entity.directory
        new ArtifactLocation.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.Recursion[S] =>
        val value = e.entity.deployed.entity.artifact.value
        new Recursion.Impl(parent, tx.newHandle(e), name, value)
      case e: Element.Code[S] =>
        val value = e.entity.value.contextName
        new Code.Impl(parent, tx.newHandle(e), name, value)
    }
  }

  // -------- String --------

  object String {
    private val icon = imageIcon("string")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.String[S]],
                                                       var name: _String, var value: _String)
      extends String[S] with ElementView.Impl[S] {

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

      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = update match {
        case Change(_, now: _String) =>
          guiFromTx(value = now)
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
    private val icon = imageIcon("integer")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Int[S]],
                                                       var name: _String, var value: _Int)
      extends Int[S] with ElementView.Impl[S] {

      def prefix = "Int"
      def icon = Int.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = {
        val numOpt = value match {
          case num: _Int  => Some(num)
          case s: _String => Try(s.toInt).toOption
        }
        numOpt.exists { num =>
          val expr    = element().entity
          val changed = expr.value != num
          if (changed) {
            val imp = ExprImplicits[S]
            import imp._
            expr() = num
          }
          changed
        }
      }

      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = update match {
        case Change(_, now: _Int) =>
          guiFromTx(value = now)
          true
        case _ => false
      }
    }
  }
  sealed trait Int[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Int[S]]
    var value: _Int
  }

  // -------- Double --------

  object Double {
    private val icon = imageIcon("float")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Double[S]],
                                                       var name: _String, var value: _Double)
      extends Double[S] with ElementView.Impl[S] {

      def prefix = "Double"
      def icon = Double.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = {
        val numOpt = value match {
          case num: _Double => Some(num)
          case s: _String => Try(s.toDouble).toOption
        }
        numOpt.exists { num =>
          val expr = element().entity
          val changed = expr.value != num
          if (changed) {
            val imp = ExprImplicits[S]
            import imp._
            expr() = num
          }
          changed
        }
      }

      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = update match {
        case Change(_, now: _Double) =>
          guiFromTx(value = now)
          true
        case _ => false
      }
    }
  }
  sealed trait Double[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Double[S]]
  }

  // -------- FolderLike --------

  sealed trait FolderLike[S <: Sys[S]] extends Renderer[S] {
    def branchID(implicit tx: S#Tx) = folder.id

    def folder(implicit tx: S#Tx): _Folder[S]

    def tryConvert(upd: Any): _Folder.Update[S] = upd match {
      case ll: LinkedList.Update[_, _, _] =>
        convert(ll.asInstanceOf[LinkedList.Update[S, Element[S], Element.Update[S]]])
      case _ => Vector.empty
    }

    def convert(upd: LinkedList.Update[S, Element[S], Element.Update[S]]): _Folder.Update[S] =
      upd.changes.map {
        case LinkedList.Added  (idx, elem)  => _Folder.Added  (idx, elem)
        case LinkedList.Removed(idx, elem)  => _Folder.Removed(idx, elem)
        case LinkedList.Element(elem, eu)   => _Folder.Element(elem, eu)
      }

    /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
    var children: Vec[ElementView[S]]

    def value {}
  }

  // -------- Group --------

  object Folder {
    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Folder[S]],
                                                       var name: _String)
      extends Folder[S] with ElementView.Impl[S] {

      var children = Vec.empty[ElementView[S]]

      def folder(implicit tx: S#Tx): _Folder[S] = element().entity

      def prefix = "Folder"
      def icon = Swing.EmptyIcon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = false  // element addition and removal is handled by folder view
    }
  }
  sealed trait Folder[S <: Sys[S]] extends FolderLike[S] with ElementView[S] /* Branch[S] */ {
    def element: stm.Source[S#Tx, Element.Folder[S]]
  }

  // -------- ProcGroup --------

  object ProcGroup {
    private val icon = imageIcon("procgroup")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.ProcGroup[S]],
                                                       var name: _String)
      extends ProcGroup[S] with ElementView.Impl[S] {

      def prefix = "ProcGroup"
      def value {}
      def icon = ProcGroup.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = false
    }
  }
  sealed trait ProcGroup[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.ProcGroup[S]]
  }

  // -------- AudioGrapheme --------

  object AudioGrapheme {
    val icon: Icon = imageIcon("audiographeme")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.AudioGrapheme[S]],
                                                       var name: _String, var value: Grapheme.Value.Audio)
      extends AudioGrapheme[S] with ElementView.Impl[S] {

      def prefix = "String"
      def icon = AudioGrapheme.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): Boolean = false

      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = update match {
        case Change(_, now: Grapheme.Value.Audio) =>
          guiFromTx(value = now)
          true
        case _ => false
      }
    }
  }
  sealed trait AudioGrapheme[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.AudioGrapheme[S]]
    var value: Grapheme.Value.Audio
  }

  // -------- ArtifactLocation --------

  object ArtifactLocation {
    private val icon = imageIcon("location")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.ArtifactLocation[S]],
                                                       var name: _String, var directory: File)
      extends ArtifactLocation[S] with ElementView.Impl[S] {

      def prefix  = "ArtifactStore"
      def value   = directory
      def icon    = ArtifactLocation.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): Boolean = false

      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = update match {
        case Artifact.Location.Moved(_, Change(_, now)) =>
          guiFromTx(directory = now)
          true
        case _ => false
      }
    }
  }
  sealed trait ArtifactLocation[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.ArtifactLocation[S]]
    var directory: File
  }

  // -------- Recursion --------

  object Recursion {
    private val icon = imageIcon("recursion")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Recursion[S]],
                                                       var name: _String, var deployed: File)
      extends Recursion[S] with ElementView.Impl[S] {

      def prefix  = "Recursion"
      def value   = deployed
      def icon    = Recursion.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = false
    }
  }
  sealed trait Recursion[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Recursion[S]]
    var deployed: File
  }

  // -------- ArtifactLocation --------

  object Code {
    private val icon = imageIcon("code")

    private[ElementView] final class Impl[S <: Sys[S]](protected val _parent: FolderLike[S],
                                                       val element: stm.Source[S#Tx, Element.Code[S]],
                                                       var name: _String, var value: _String)
      extends Code[S] with ElementView.Impl[S] {

      def prefix  = "Code"
      def icon    = Code.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean = false
    }
  }
  sealed trait Code[S <: Sys[S]] extends ElementView[S] {
    def element: stm.Source[S#Tx, Element.Code[S]]
    var value: _String
  }

  // -------- Root --------

  object Root {
    private[gui] def apply[S <: Sys[S]](folder: _Folder[S])(implicit tx: S#Tx): Root[S] = {
      import _Folder.serializer
      val res = new Impl(tx.newHandle(folder))
      // res.children = folder.iterator.map(ElementView(res, _)(tx)).toIndexedSeq
      res
    }

    private final class Impl[S <: Sys[S]](handle: stm.Source[S#Tx, _Folder[S]])
      extends Root[S] {

      var children = Vec.empty[ElementView[S]]
      def folder(implicit tx: S#Tx): _Folder[S] = handle()
      def name = "root"
      def parent: Option[FolderLike[S]] = None
      def icon = Swing.EmptyIcon

      def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean = false

      override def toString = "ElementView.Root"
    }
  }
  sealed trait Root[S <: Sys[S]] extends FolderLike[S]

  private def imageIcon(name: _String): Icon = new ImageIcon(
    Toolkit.getDefaultToolkit.getImage(Mellite.getClass.getResource(s"icon_${name}16.png"))
  )

  private sealed trait Impl[S <: Sys[S]] extends ElementView[S] {
    protected def prefix: _String
    override def toString = s"ElementView.$prefix(name = $name)"
    protected def _parent: FolderLike[S]

    def parent: Option[FolderLike[S]] = Some(_parent)
  }

  sealed trait Renderer[S <: Sys[S]] {
    // private[gui] def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
    def name: _String
    def parent: Option[ElementView.FolderLike[S]]
    def value: Any
    def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean
    def icon: Icon
  }
}
sealed trait ElementView[S <: Sys[S]] extends ElementView.Renderer[S] {
  def element: stm.Source[S#Tx, Element[S]]
  var name: String
  def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean
  // def parent: Option[ElementView.FolderLike[S]]
}