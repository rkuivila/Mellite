/*
 *  DocumentFrameImpl.scala
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

import swing.{Orientation, SplitPane, FlowPanel, Action, Button, BorderPanel, Frame}
import de.sciss.lucre.stm.{Disposable, Sys}
import javax.swing.WindowConstants
import de.sciss.synth.proc.{Transport, Proc, ProcGroupX}
import de.sciss.synth.expr.SpanLikes

object DocumentFrameImpl {
   def apply[ S <: Sys[ S ]]( doc: Document[ S ])( implicit tx: S#Tx ) : DocumentFrame[ S ] = {
      implicit val csr = doc.cursor
      implicit val groupsSer  = Document.Serializers.groups[ S ]
      implicit val transpSer  = Document.Serializers.transports[ S ]
      val groupsView = ListView( doc.groups )( g => "Timeline " + g.id )
      val transpView = ListView.empty[ S, Transport[ S, Proc[ S ]], Unit ]( _.toString() )
      val view = new Impl( doc, groupsView, transpView )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( val document: Document[ S ],
                                             groupsView: ListView[ S, Document.Group[ S ], Document.GroupUpdate[ S ]],
                                             transpView: ListView[ S, Transport[ S, Proc[ S ]], Unit ])
   extends DocumentFrame[ S ] {
      private var comp: Frame = _

      private def atomic[ A ]( fun: S#Tx => A ) : A = document.cursor.step( fun )

      def component: Frame = {
         requireEDT()
         val res = comp
         if( res == null ) sys.error( "Called component before GUI was initialized" )
         res
      }

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val ggAddGroup = Button( "+" ) {
            atomic { implicit tx =>
               implicit val spans = SpanLikes
               val group = ProcGroupX.Modifiable[ S ]
               document.groups.addLast( group )
            }
         }

         def mkDelButton[ Elem <: Disposable[ S#Tx ], U ]( view: ListView[ S, Elem, U ]): Button =
            new Button( Action( "\u2212" ) {
               val indices = view.guiSelection
               if( indices.nonEmpty ) atomic { implicit tx =>
                  view.list.flatMap( _.modifiableOption ).foreach { ll =>
                     val sz   = ll.size
                     val ind1 = indices.filter( _ < sz ).sortBy( -_ )
                     ind1.foreach { idx =>
                        ll.removeAt( idx ).dispose()
                     }
                  }
               }
            }) {
               enabled = false
            }

         val ggDelGroup = mkDelButton( groupsView )

         val ggViewGroup = new Button( Action( "View" ) {

         }) {
            enabled = false
         }

         val groupsButPanel = new FlowPanel( ggAddGroup, ggDelGroup, ggViewGroup )

         val ggAddTransp = new Button( Action( "+" ) {
            println( "Add Transport" )
         }) {
            enabled = false
         }

         val ggDelTransp = mkDelButton( transpView )

         val transpButPanel = new FlowPanel( ggAddTransp, ggDelTransp )

         val groupsPanel = new BorderPanel {
            add( groupsView.component, BorderPanel.Position.Center )
            add( groupsButPanel, BorderPanel.Position.South )
         }

         val transpPanel = new BorderPanel {
            add( transpView.component, BorderPanel.Position.Center )
            add( transpButPanel, BorderPanel.Position.South )
         }

         groupsView.guiReact {
            case ListView.SelectionChanged( indices ) =>
//               println( "SELECTION " + indices )
               val isSelected = indices.nonEmpty
               ggDelGroup.enabled  = isSelected
               ggViewGroup.enabled = isSelected
               val isSingle = indices.size == 1
               ggAddTransp.enabled = isSingle
               ggDelTransp.enabled = false
               atomic { implicit tx =>
                  val transpList = if( isSingle ) {
                     for( ll <- groupsView.list; idx <- indices.headOption; group <- ll.get( idx ))
                        yield document.transports( group )
//                     val groupOption = groupsView.list.flatMap( _.get( indices.head ))
//                     groupOption.map( group => document.transports( group ))
                  } else {
                     None
                  }
                  transpView.list_=( transpList )
               }
         }

         comp = new Frame {
            title    = "Document : " + document.folder.getName
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            contents = new SplitPane( Orientation.Horizontal, groupsPanel, transpPanel )
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
