/*
 *  FolderViewImpl.scala
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

import de.sciss.synth.proc.Sys
import swing.{ScrollPane, Component}
import collection.breakOut
import collection.immutable.{IndexedSeq => IIdxSeq}
import scalaswingcontrib.tree.{TreeModel, Tree}
import scalaswingcontrib.event.TreePathSelected
import de.sciss.lucre.stm.{IdentifierMap, Disposable}
import de.sciss.model.impl.ModelImpl

object FolderViewImpl {
  private final val DEBUG = false

  private type Path[S <: Sys[S]] = Tree.Path[ElementView.Branch[S]]

  def apply[S <: Sys[S]](root: Folder[S])(implicit tx: S#Tx): FolderView[S] = {
    val rootView  = ElementView.Root(root)
    val map       = tx.newInMemoryIDMap[Disposable[S#Tx]] // folders to observers
    val view      = new Impl[S](rootView, map)

    def loop(path: Tree.Path[ElementView.Folder[S]], gl: ElementView.BranchLike[S]) {
      // val g = gl.folder
      view.branchAdded(path, gl)
      gl.children.foreach {
        case c: ElementView.Folder[S] => loop(path :+ c, c)
        case _ =>
      }
    }
    loop(Tree.Path.empty, rootView)

    guiFromTx {
      view.guiInit()
    }

    view
  }

  private final class Renderer[S <: Sys[S]] extends Tree.Renderer[ElementView[S]] {
    def componentFor(owner: Tree[_], value: ElementView[S], cellInfo: Tree.Renderer.CellInfo): Component = {
      value.componentFor(owner, cellInfo)
    }
  }

  private final class Impl[S <: Sys[S]](root: ElementView.Root[S],
                                        mapBranches: IdentifierMap[S#ID, S#Tx, Disposable[S#Tx]])
    extends FolderView[S] with ModelImpl[FolderView.Update[S]] {
    view =>

    @volatile private var comp: Component = _
    private var _model: TreeModel[ElementView[S]] = _
    def model = _model
    private var t: Tree[ElementView[S]] = _

    def elemAdded(parent: Tree.Path[ElementView.Folder[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"elemAdded = $parent $idx $elem")
      val v = ElementView(elem)

      guiFromTx {
        if (DEBUG) println(s"model.insertUnder($parent, $v, $idx)")
        val g       = parent.lastOption.getOrElse(root)
        require(idx >= 0 && idx <= g.children.size)
        g.children  = g.children.patch(idx, Vector(v), 0)
        val res     = model.insertUnder(parent, v, idx)
        require(res)
        // if (parent.isEmpty && idx == 0) t.expandPath(parent)  // stupid bug where new elements on root level are invisible
      }

      (elem, v) match {
        case (g: Element.Folder[S], gv: ElementView.Folder[S]) =>
          val cg    = g.entity
          val path  = parent :+ gv
          branchAdded(path, gv)
          if (!cg.isEmpty) {
            cg.iterator.toList.zipWithIndex.foreach { case (c, ci) =>
              elemAdded(path, ci, c)
            }
          }

        case _ =>
      }
    }

    def elemRemoved(parent: Tree.Path[ElementView.Folder[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"elemRemoved = $parent $idx $elem")
      val v = parent.lastOption.getOrElse(root).children(idx)
      elemViewRemoved(parent, v, elem)
    }

    private def elemViewRemoved(parent: Tree.Path[ElementView.Folder[S]], v: ElementView[S],
                                elem: Element[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"elemViewRemoved = $parent $v $elem")
      (elem, v) match {
        case (g: Element.Folder[S], gl: ElementView.Folder[S]) =>
          mapBranches.get(elem.id).foreach(_.dispose()) // child observer
          mapBranches.remove(elem.id)
          val path = parent :+ gl
          g.entity.iterator.toList.zipWithIndex.reverse.foreach { case (c, ci) =>
            elemRemoved(path, ci, c)
          }

        case _ =>
      }
      // println(s"elemRemoved = $path $idx $elem")
//      val viewOpt = map.get(elem.id)
//      if (viewOpt.isDefined) map.remove(elem.id)
      guiFromTx {
        val path    = parent :+ v
        val g       = parent.lastOption.getOrElse(root)
        val idx     = g.children.indexOf(v)
        if (DEBUG) println(s"model.remove($path) = idx $idx")
        require(idx >= 0 && idx < g.children.size)
        val res     = model.remove(path)
        require(res)
        g.children  = g.children.patch(idx, Vector.empty, 1)
      }
    }

    def dispose()(implicit tx: S#Tx) {
      val emptyPath = Tree.Path.empty
      root.children.foreach { v => elemViewRemoved(emptyPath, v, v.element()) }
      val r = root.folder
      mapBranches.get(r.id).foreach(_.dispose())
      mapBranches.remove(r.id)
      mapBranches.dispose()
    }

    /** Register a new sub folder for observation.
      *
      * @param path     the path up to and including the folder (exception: root is not included)
      * @param branch   the folder to observe
      */
    def branchAdded(path: Tree.Path[ElementView.Folder[S]], branch: ElementView.BranchLike[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"branchAdded: $path $branch")
      val obs = branch.react { implicit tx => upd =>
        // println(s"List update. toSeq = ${upd.list.iterator.toIndexedSeq}")
        upd.foreach {
          case Folder.Added  (idx, elem)      => elemAdded  (path, idx, elem)
          case Folder.Removed(idx, elem)      => elemRemoved(path, idx, elem)
          case Folder.Element(elem, elemUpd)  => println(s"Warning: FolderView unhandled $upd")
          // case _ =>
        }
      }
      mapBranches.put(branch.branchID, obs)
    }

    def component: Component = {
      requireEDT()
      val res = comp
      if (res == null) sys.error("Called component before GUI was initialized")
      res
    }

    def guiInit() {
      requireEDT()
      require(comp == null, "Initialization called twice")

      _model = new TreeModelImpl[ElementView[S]](root.children, {
        case g: ElementView.BranchLike[S] => g.children
        case _ => Vector.empty
      })

      t = new Tree(_model)
      t.listenTo(t.selection)
      t.reactions += {
        case TreePathSelected(_, _, _,_, _) =>  // this crappy untyped event doesn't help us at all
          dispatch(FolderView.SelectionChanged(view, selection))
      }
      t.showsRootHandles = true
      t.renderer = new Renderer[S]
      // t.expandAll()
      t.expandPath(Tree.Path.empty)

      val scroll    = new ScrollPane(t)
      scroll.border = null
      comp = scroll
    }

    def selection: FolderView.Selection[S] =
      if (t.selection.isEmpty) Vector.empty   // WARNING: currently we get a NPE if accessing `paths` on an empty selection
      else t.selection.paths.collect({
        case PathExtrator(path, child) => (path, child)
      })(breakOut)

    object PathExtrator {
      def unapply(path: Seq[ElementView[S]]): Option[(IIdxSeq[ElementView.BranchLike[S]], ElementView[S])] =
        path match {
          case init :+ last =>
            val pre: IIdxSeq[ElementView.BranchLike[S]] = init.map({
              case g: ElementView.BranchLike[S] => g
              case _ => return None
            })(breakOut)
            Some((root +: pre, last))
          case _ => None
        }
    }
  }
}