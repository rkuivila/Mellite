/*
 *  ListViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.stm.{Txn, Disposable, Sys}
import de.sciss.lucre.expr.LinkedList
import swing.{ScrollPane, Component}
import javax.swing.DefaultListModel
import collection.immutable.{IndexedSeq => IIdxSeq}
import concurrent.stm.{Ref => STMRef}
import swing.event.ListSelectionChanged

object ListViewImpl {
   def apply[ S <: Sys[ S ], Elem ]( list: LinkedList[ S, Elem, _ ])( show: Elem => String )
                                   ( implicit tx: S#Tx ) : ListView[ S ] = {
      val view    = new Impl[ S ]
      val items   = list.iterator.toIndexedSeq
      guiFromTx {
         view.guiInit()
         view.addAll( items.map( show ))
      }
      val obs = list.changed.reactTx { implicit tx => {
         case LinkedList.Added(   _, idx, elem )   => guiFromTx( view.add( idx, show( elem )))
         case LinkedList.Removed( _, idx, elem )   => guiFromTx( view.remove( idx ))
         case LinkedList.Element( li, upd )        =>
            val ch = upd.foldLeft( Map.empty[ Int, String ]) { case (map0, (elem, _)) =>
               val idx = li.indexOf( elem )
               if( idx >= 0 ) {
                  map0 + (idx -> show( elem ))
               } else map0
            }
            guiFromTx {
               ch.foreach { case (idx, str) => view.update( idx, str )}
            }
      }}
      view.observer = obs
      view
   }

   private final class Impl[ S <: Sys[ S ]] extends ListView[ S ] {
      @volatile private var comp: Component = _
      @volatile private var ggList: swing.ListView[ _ ] = _

      private val mList  = new DefaultListModel
      var observer: Disposable[ S#Tx ] = _

      private var viewObservers = IIdxSeq.empty[ Observer ]

      private final class Observer( fun: PartialFunction[ ListView.Update, Unit ]) extends Removable {
         obs =>

         def remove() {
            viewObservers = viewObservers.filterNot( _ == obs )
         }

         def tryApply( evt: ListView.Update ) {
            try {
               if( fun.isDefinedAt( evt )) fun( evt )
            } catch {
               case e: Exception => e.printStackTrace()
            }
         }
      }

      private def notifyViewObservers( current: IIdxSeq[ Int ]) {
         val evt = ListView.SelectionChanged( current )
         viewObservers.foreach( _.tryApply( evt ))
      }

      def component: Component = {
         requireEDT()
         val res = comp
         if( res == null ) sys.error( "Called component before GUI was initialized" )
         res
      }

      def guiReact( fun: PartialFunction[ ListView.Update, Unit ]) : Removable = {
         requireEDT()
         val obs = new Observer( fun )
         viewObservers :+= obs
         obs
      }

      def guiSelection : IIdxSeq[ Int ] = {
         requireEDT()
         ggList.selection.indices.toIndexedSeq
      }

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )
//         val rend = new DefaultListCellRenderer {
//            override def getListCellRendererComponent( c: JList, elem: Any, idx: Int, selected: Boolean, focused: Boolean ) : awt.Component = {
//               super.getListCellRendererComponent( c, showFun( elem.asInstanceOf[ Elem ]), idx, selected, focused )
//            }
//         }
         ggList = new swing.ListView {
            peer.setModel( mList )
            listenTo( selection )
            reactions += {
               case l: ListSelectionChanged[ _ ] => notifyViewObservers( l.range )
            }
         }

         comp = new ScrollPane( ggList )
      }

      def addAll( items: IIdxSeq[ String ]) {
         items.foreach( mList.addElement _ )
      }

      def add( idx: Int, item: String ) {
         mList.add( idx, item )
      }

      def remove( idx: Int ) {
         mList.remove( idx )
      }

      def update( idx: Int, item: String ) {
         mList.set( idx, item )
      }

      def dispose()( implicit tx: S#Tx ) {
         observer.dispose()
         guiFromTx {
            mList.clear()
            viewObservers = IIdxSeq.empty
         }
      }
   }
}
