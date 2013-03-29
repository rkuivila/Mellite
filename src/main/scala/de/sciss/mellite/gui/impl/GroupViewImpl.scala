/*
 *  GroupViewImpl.scala
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
import swing.{Swing, Orientation, BoxPanel, Label, ScrollPane, Component}
import collection.immutable.{IndexedSeq => IIdxSeq}
import scalaswingcontrib.tree.{ExternalTreeModel, Tree}
import Swing._
import javax.swing.tree.DefaultTreeCellRenderer
import de.sciss.desktop.impl.ModelImpl
import scalaswingcontrib.event.TreePathSelected
import de.sciss.lucre.stm

object GroupViewImpl {
  def apply[S <: Sys[S]](root: Elements[S])(implicit tx: S#Tx): GroupView[S] = {
    val view      = new Impl[S]
    val rootView  = ElementView.Root(root)

    guiFromTx {
      view.guiInit(rootView)
    }

    def elemAdded(path: Tree.Path[ElementView.Group[S]], idx: Int, elem: Element[S])(implicit tx: S#Tx) {
      println(s"elemAdded = $path $idx $elem")
      val v = ElementView(elem)
      guiFromTx {
        // parentView.children = paren  tView.children.patch(idx, IIdxSeq(elemView), 0)
        view.model.insertUnder(path, v, idx)
      }
    }

    root.changed.reactTx[Elements.Update[S]] {
      implicit tx => upd =>
        println(s"List update. toSeq = ${upd.list.iterator.toIndexedSeq}")
        upd.changes.foreach {
          case Elements.Added  (idx, elem)      => elemAdded(Tree.Path.empty, idx, elem)
          case Elements.Removed(idx, elem)      => ???
          case Elements.Element(elem, elemUpd)  => ???
          //        case _ =>
        }
      //      case Element.Update(elem, changes) =>
      //      case Elements.Update(root0, changes) =>
      //
      //      case _ =>
    }

    view
  }

//  private final val cmpBlank  = new Label
//  private final val cmpLabelJ = new DefaultTreeCellRenderer
//  cmpLabelJ.setLeafIcon(null)
//  private final val cmpLabel  = Component.wrap(cmpLabelJ)
//  private final val cmpGroupJ = new DefaultTreeCellRenderer
//  private final val cmpGroup  = Component.wrap(cmpGroupJ)

  private final class Renderer[S <: Sys[S]] extends Tree.Renderer[ElementView[S]] {
    def componentFor(owner: Tree[_], value: ElementView[S], cellInfo: Tree.Renderer.CellInfo): Component = {
      value.componentFor(owner, cellInfo)
    }
  }

  private final class Impl[S <: Sys[S]] extends GroupView[S] with ModelImpl[GroupView.Update[S]] {
    view =>

    @volatile private var comp: Component = _
    private var _model: ExternalTreeModel[ElementView[S]] = _
    def model = _model

    def component: Component = {
      requireEDT()
      val res = comp
      if (res == null) sys.error("Called component before GUI was initialized")
      res
    }

    def guiInit(root: ElementView.Root[S]) {
      requireEDT()
      require(comp == null, "Initialization called twice")
      //       ggList = new swing.ListView {
      //          peer.setModel( mList )
      //          listenTo( selection )
      //          reactions += {
      //             case l: ListSelectionChanged[ _ ] => notifyViewObservers( l.range )
      //          }
      //       }

      _model = ExternalTreeModel[ElementView[S]](root.children: _*) {
        case g: ElementView.GroupLike[S] => g.children
        case _ => Nil
      } makeInsertableWith { (path, elem, idx) =>
        path.lastOption.getOrElse(root) match {
          case g: ElementView.GroupLike[S] if g.children.size >= idx =>
            g.children = g.children.patch(idx, IIdxSeq(elem), 0)
            //            println(s"Expanding ${g} at ${idx} with ${elem} - now children are ${g.children}")
            true
          case _ => false
        }
      }

      val t = new Tree(_model)
      t.listenTo(t.selection)
      t.reactions += {
        case TreePathSelected(_, pathsAdded, pathsRemoved,_, _) =>
          ???
//          dispatch(GroupView.SelectionChanged(view, allPaths))
      }
      t.showsRootHandles = true
      t.renderer = new Renderer[S]
      t.expandAll()

      comp = new ScrollPane(t)
    }
  }
}