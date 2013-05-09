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
import de.sciss.lucre.stm.{Cursor, IdentifierMap, Disposable}
import de.sciss.model.impl.ModelImpl
import de.sciss.synth.expr.ExprImplicits
import de.sciss.mellite.gui.TreeTableCellRenderer.{State, TreeState}
import scala.util.control.NonFatal

object FolderViewImpl {
  private final val DEBUG = false

  private type Path[S <: Sys[S]] = Tree.Path[ElementView.Folder[S]]

  def apply[S <: Sys[S]](root: Folder[S])(implicit tx: S#Tx, cursor: Cursor[S]): FolderView[S] = {
    val rootView  = ElementView.Root(root)
    val map       = tx.newInMemoryIDMap[Disposable[S#Tx]] // folders to observers
    val view      = new Impl[S](rootView, map)

    def loop(parentPath: Tree.Path[ElementView.FolderLike[S]], gl: ElementView.FolderLike[S]) {
      // val g = gl.folder
      val path = parentPath :+ gl
      view.branchAdded(path, gl)
      gl.children.foreach {
        case c: ElementView.Folder[S] => loop(path, c)
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

  private final class Editor[S <: Sys[S]] extends Tree.Editor[ElementView[S]] {
    def componentFor(owner: Tree[_], value: ElementView[S], cellInfo: Tree.Editor.CellInfo): Component = {
      println("---editor---")
      null
    }

    def value: ElementView[S] = null
  }

  private final class Impl[S <: Sys[S]](_root: ElementView.Root[S],
                                        mapBranches: IdentifierMap[S#ID, S#Tx, Disposable[S#Tx]])
                                       (implicit cursor: Cursor[S])
    extends FolderView[S] with ModelImpl[FolderView.Update[S]] {
    view =>

    type Node   = ElementView.Renderer[S]
    type Branch = ElementView.FolderLike[S]

    private class ElementTreeModel extends AbstractTreeModel[Node] {
      lazy val root: Node = _root // ! must be lazy. suckers....

      def getChildCount(parent: Node): Int = parent match {
        case b: ElementView.FolderLike[S] => b.children.size
        case _ => 0
      }

      def getChild(parent: Node, index: Int): ElementView[S] = parent match {
        case b: ElementView.FolderLike[S] => b.children(index)
        case _ => sys.error(s"parent $parent is not a branch")
      }

      def isLeaf(node: Node): Boolean = node match {
        case b: ElementView.FolderLike[S] => false // b.children.nonEmpty
        case _ => true
      }

      def getIndexOfChild(parent: Node, child: Node): Int = parent match {
        case b: ElementView.FolderLike[S] => b.children.indexOf(child)
        case _ => sys.error(s"parent $parent is not a branch")
      }

      def getParent(node: Node): Option[Node] = node.parent

      def valueForPathChanged(path: TreeTable.Path[Node], newValue: Node) {
        println(s"valueForPathChanged($path, $newValue)")
      }

      def elemAdded(parent: Tree.Path[ElementView.FolderLike[S]], idx: Int, view: ElementView[S]) {
        if (DEBUG) println(s"model.insertUnder($parent, $idx, $view)")
        val g       = parent.last // Option.getOrElse(_root)
        require(idx >= 0 && idx <= g.children.size)
        g.children  = g.children.patch(idx, Vector(view), 0)
        fireNodesInserted(view)
      }

      def elemRemoved(parent: Tree.Path[ElementView.FolderLike[S]], idx: Int) {
        val g = parent.last // Option.getOrElse(_root)
        if (DEBUG) println(s"model.remove($parent, $idx)")
        require(idx >= 0 && idx < g.children.size)
        val v       = g.children(idx)
        g.children  = g.children.patch(idx, Vector.empty, 1)
        fireNodesRemoved(v)
      }
    }

    @volatile private var comp: Component = _
    private var _model: ElementTreeModel  = _
    // def model = _model
    // private var t: Tree[ElementView[S]] = _
    private var t: TreeTable[Node, TreeColumnModel[Node]] = _

    def elemAdded(parent: Tree.Path[ElementView.FolderLike[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"elemAdded = $parent $idx $elem")
      val v = ElementView(parent.last, elem)

      guiFromTx {
        _model.elemAdded(parent, idx, v)
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

    //    def elemRemoved(parent: Tree.Path[ElementView.FolderLike[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
    //      if (DEBUG) println(s"elemRemoved = $parent $idx $elem")
    //      val v = parent.last // Option.getOrElse(_root).children(idx)
    //      elemViewRemoved(parent, v, elem)
    //    }

    def elemRemoved(parent: Tree.Path[ElementView.FolderLike[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"elemViewRemoved = $parent $elem")
      val v = parent.last.children(idx) // XXX TODO: not good, shouldn't access this potentially outside GUI thread
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
        _model.elemRemoved(parent, idx)
      }
    }

    def dispose()(implicit tx: S#Tx) {
      val rootPath = Tree.Path(_root)
      // XXX TODO: should not access children potentially outside GUI thread. Use linked list instead
      _root.children.zipWithIndex.reverse.foreach { case (v, idx) => elemRemoved(rootPath, idx, v.element()) }
      val r = _root.folder
      mapBranches.get(r.id).foreach(_.dispose())
      mapBranches.remove(r.id)
      mapBranches.dispose()
    }

    /** Register a new sub folder for observation.
      *
      * @param path     the path up to and including the folder (exception: root is not included)
      * @param branch   the folder to observe
      */
    def branchAdded(path: Tree.Path[ElementView.FolderLike[S]], branch: ElementView.FolderLike[S])(implicit tx: S#Tx) {
      if (DEBUG) println(s"branchAdded: $path $branch")
      val obs = branch.react { implicit tx => upd =>
        // println(s"List update. toSeq = ${upd.list.iterator.toIndexedSeq}")
        upd.foreach {
          case Folder.Added  (idx, elem)      => elemAdded  (path, idx, elem)
          case Folder.Removed(idx, elem)      => elemRemoved(path, idx, elem)
          case Folder.Element(elem, elemUpd)  => // println(s"Warning: FolderView unhandled $upd")
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

      //      _model = new TreeModelImpl[ElementView[S]](root.children, {
      //        case g: ElementView.FolderLike[S] => g.children
      //        case _ => Vector.empty
      //      })

      _model = new ElementTreeModel

      val colName = new TreeColumnModel.Column[Node, String]("Name") {
        def apply(node: Node): String = node.name

        def update(node: Node, value: String) {
          node match {
            case v: ElementView[S] if (value != v.name) =>
              cursor.step { implicit tx =>
                val expr = ExprImplicits[S]
                import expr._
                v.element().name() = value
              }
            case _ =>
          }
        }

        def isEditable(node: Node) = node match {
          case b: ElementView[S] => true
          case _ => false // i.e. Root
        }
      }

      val colValue = new TreeColumnModel.Column[Node, Any]("Value") {
        def apply(node: Node): Any = node.value
        def update(node: Node, value: Any) {}
        def isEditable(node: Node) = node match {
          case b: ElementView.FolderLike[S] => false
          case _ => true
        }
      }

      val tcm = new TreeColumnModel.Tuple2[Node, String, Any](colName, colValue) {
        def getParent(node: Node): Option[Node] = node.parent
      }

      t = new TreeTable(_model, tcm)
      t.rootVisible = false
      t.renderer    = new TreeTableCellRenderer {
        private val component = TreeTableCellRenderer.Default
        def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int,
                                 state: State): Component = {
          val value1 = if (value != ()) value else null
          val res = component.getRendererComponent(treeTable, value1, row = row, column = column, state = state)
          if (row >= 0) state.tree match {
            case Some(TreeState(false, true)) =>
              // println(s"row = $row, col = $column")
              try {
                val node = t.getNode(row)
                component.icon = node.icon
              } catch {
                case NonFatal(_) => // XXX TODO -- currently NPE probs; seems renderer is called before tree expansion with node missing
              }
            case _ =>
          }
          res // component
        }
      }
      t.listenTo(t.selection)
      t.reactions += {
        case TreePathSelected(_, _, _,_, _) =>  // this crappy untyped event doesn't help us at all
          dispatch(FolderView.SelectionChanged(view, selection))
      }
      t.showsRootHandles = true
      //      t.peer.setDefaultRenderer(classOf[String], new TreeTableCellRenderer {
      //
      //      })
      // t.renderer  = new Renderer[S]
      // t.editor    = new Editor[S]
      // t.expandAll()
      t.expandPath(Tree.Path.empty)

      val scroll    = new ScrollPane(t)
      scroll.border = null
      comp          = scroll
    }

    def selection: FolderView.Selection[S] =
      t.selection.paths.collect({
        case PathExtrator(path, child) => (path, child)
      })(breakOut)

    object PathExtrator {
      def unapply(path: Seq[Node]): Option[(IIdxSeq[ElementView.FolderLike[S]], ElementView[S])] =
        path match {
          case init :+ (last: ElementView[S]) =>
            val pre: IIdxSeq[ElementView.FolderLike[S]] = init.map({
              case g: ElementView.FolderLike[S] => g
              case _ => return None
            })(breakOut)
            Some((/* _root +: */ pre, last))
          case _ => None
        }
    }
  }
}