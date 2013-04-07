/*
 *  ListViewImpl.scala
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

import de.sciss.lucre.{stm, expr}
import stm.{Source, Cursor, Disposable, Sys}
import expr.LinkedList
import swing.{ScrollPane, Component}
import javax.swing.DefaultListModel
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Ref => STMRef}
import swing.event.ListSelectionChanged
import de.sciss.serial.Serializer

object ListViewImpl {
  def empty[S <: Sys[S], Elem, U](show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]]): ListView[S, Elem, U] = {
    val view = new Impl[S, Elem, U](show)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  def apply[S <: Sys[S], Elem, U](list: LinkedList[S, Elem, U])(show: Elem => String)
                                 (implicit tx: S#Tx, cursor: Cursor[S],
                                  serializer: Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]]): ListView[S, Elem, U] = {
    val view = empty[S, Elem, U](show)
    view.list_=(Some(list))
    view
  }

  private final class Impl[S <: Sys[S], Elem, U](show: Elem => String)
                                                (implicit cursor: Cursor[S], listSer: Serializer[S#Tx, S#Acc, LinkedList[S, Elem, U]])
    extends ListView[S, Elem, U] {
    view =>

    @volatile private var comp: Component = _
    @volatile private var ggList: swing.ListView[_] = _

    private val mList = new DefaultListModel

    private var viewObservers = IIdxSeq.empty[Observer]

    // private val current = STMRef( Option.empty[ (S#Acc, LinkedList[ S, Elem, U ], Disposable[ S#Tx ])])
    private val current = STMRef(Option.empty[(Source[S#Tx, LinkedList[S, Elem, U]], Disposable[S#Tx])])

    def list(implicit tx: S#Tx): Option[LinkedList[S, Elem, U]] = {
      current.get(tx.peer).map {
        case (h, _) => h()
      }
    }

    def list_=(newOption: Option[LinkedList[S, Elem, U]])(implicit tx: S#Tx) {
      current.get(tx.peer).foreach {
        case (_, obs) =>
          disposeObserver(obs)
      }
      val newValue = newOption.map {
        case ll =>
          val obs = createObserver(ll)
          (tx.newHandle(ll), obs)
      }
      current.set(newValue)(tx.peer)
    }

    private final class Observer(fun: PartialFunction[ListView.Update, Unit]) extends Removable {
      obs =>

      def remove() {
        viewObservers = viewObservers.filterNot(_ == obs)
      }

      def tryApply(evt: ListView.Update) {
        try {
          if (fun.isDefinedAt(evt)) fun(evt)
        } catch {
          case e: Exception => e.printStackTrace()
        }
      }
    }

    private def disposeObserver(obs: Disposable[S#Tx])(implicit tx: S#Tx) {
      obs.dispose()
      guiFromTx {
        view.clear()
      }
    }

    private def createObserver(ll: LinkedList[S, Elem, U])(implicit tx: S#Tx): Disposable[S#Tx] = {
      val items = ll.iterator.toIndexedSeq
      guiFromTx {
        view.addAll(items.map(show))
      }
      ll.changed.reactTx[LinkedList.Update[S, Elem, U]] {
        implicit tx => {
          upd =>
          //            case LinkedList.Added(   _, idx, elem )   => guiFromTx( view.add( idx, show( elem )))
          //            case LinkedList.Removed( _, idx, elem )   => guiFromTx( view.remove( idx ))
          //            case LinkedList.Element( li, upd )        =>
          //               val ch = upd.foldLeft( Map.empty[ Int, String ]) { case (map0, (elem, _)) =>
          //                  val idx = li.indexOf( elem )
          //                  if( idx >= 0 ) {
          //                     map0 + (idx -> show( elem ))
          //                  } else map0
          //               }
          //               guiFromTx {
          //                  ch.foreach { case (idx, str) => view.update( idx, str )}
          //               }
        }
      }
    }

    private def notifyViewObservers(current: IIdxSeq[Int]) {
      val evt = ListView.SelectionChanged(current)
      viewObservers.foreach(_.tryApply(evt))
    }

    def component: Component = {
      requireEDT()
      val res = comp
      if (res == null) sys.error("Called component before GUI was initialized")
      res
    }

    def guiReact(fun: PartialFunction[ListView.Update, Unit]): Removable = {
      requireEDT()
      val obs = new Observer(fun)
      viewObservers :+= obs
      obs
    }

    def guiSelection: IIdxSeq[Int] = {
      requireEDT()
      ggList.selection.indices.toIndexedSeq
    }

    def guiInit() {
      requireEDT()
      require(comp == null, "Initialization called twice")
      //         val rend = new DefaultListCellRenderer {
      //            override def getListCellRendererComponent( c: JList, elem: Any, idx: Int, selected: Boolean, focused: Boolean ) : awt.Component = {
      //               super.getListCellRendererComponent( c, showFun( elem.asInstanceOf[ Elem ]), idx, selected, focused )
      //            }
      //         }
      ggList = new swing.ListView {
        peer.setModel(mList)
        listenTo(selection)
        reactions += {
          case l: ListSelectionChanged[_] => notifyViewObservers(l.range)
        }
      }

      comp = new ScrollPane(ggList)
    }

    def clear() {
      mList.clear()
    }

    def addAll(items: IIdxSeq[String]) {
      mList.clear()
      items.foreach(mList.addElement _)
    }

    def add(idx: Int, item: String) {
      mList.add(idx, item)
    }

    def remove(idx: Int) {
      mList.remove(idx)
    }

    def update(idx: Int, item: String) {
      mList.set(idx, item)
    }

    def dispose()(implicit tx: S#Tx) {
      list_=(None)
      guiFromTx {
        viewObservers = IIdxSeq.empty
      }
    }
  }
}
