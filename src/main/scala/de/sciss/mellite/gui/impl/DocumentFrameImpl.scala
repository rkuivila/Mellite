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
import de.sciss.lucre.stm.{Cursor, Disposable, Sys}
import javax.swing.WindowConstants
import de.sciss.synth.proc.{AuralPresentation, Transport, Proc, ProcGroupX}
import de.sciss.synth.expr.SpanLikes

object DocumentFrameImpl {
   def apply[ S <: Sys[ S ]]( doc: Document[ S ])( implicit tx: S#Tx ) : DocumentFrame[ S ] = {
      implicit val csr = doc.cursor
      implicit val groupsSer  = Document.Serializers.groups[ S ]
      implicit val transpSer  = Document.Serializers.transports[ S ]
      val groupsView = ListView( doc.groups )( g => "Group " + g.id )
      val transpView = ListView.empty[ S, Transport[ S, Proc[ S ]], Unit ]( _.toString() )
      val view = new Impl( doc, groupsView, transpView )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( val document: Document[ S ],
                                             groupsView: ListView[ S, Document.Group[ S ], Document.GroupUpdate[ S ]],
                                             transpView: ListView[ S, Document.Transport[ S ], Unit ])
   extends DocumentFrame[ S ] with ComponentHolder[ Frame ] with CursorHolder[ S ] {
      protected implicit def cursor: Cursor[ S ] = document.cursor

      private def transport( implicit tx: S#Tx ) : Option[ Document.Transport[ S ]] = {
         for( gl <- groupsView.list; gidx <- groupsView.guiSelection.headOption; group <- gl.get( gidx );
              tl <- transpView.list; tidx <- transpView.guiSelection.headOption; transp <- document.transports( group ).get( tidx ))
            yield transp
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

         val ggViewTimeline = new Button( Action( "View Timeline" ) {

         }) {
            enabled = false
         }

         val groupsButPanel = new FlowPanel( ggAddGroup, ggDelGroup, ggViewTimeline )

         val ggAddTransp = new Button( Action( "+" ) {
            atomic { implicit tx =>
               for( ll <- groupsView.list; idx <- groupsView.guiSelection.headOption; group <- ll.get( idx )) {
                  val transp = Transport( group )
                  document.transports( group ).addLast( transp )
               }
            }
         }) {
            enabled = false
         }

         val ggDelTransp = mkDelButton( transpView )

         val ggViewInstant = new Button( Action( "View Instant" ) {
            atomic { implicit tx =>
               for( gl <- groupsView.list; gidx <- groupsView.guiSelection.headOption; group <- gl.get( gidx );
                    tidx <- transpView.guiSelection.headOption;
                    transp <- document.transports( group ).get( tidx )) {

                  InstantGroupFrame( group, transp )
               }
            }
         }) {
            enabled = false
         }

         val transpButPanel = new FlowPanel( ggAddTransp, ggDelTransp, ggViewInstant )

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
               ggDelGroup.enabled      = isSelected
               ggViewTimeline.enabled  = isSelected
               val isSingle = indices.size == 1
               ggAddTransp.enabled = isSingle
               ggDelTransp.enabled = false
               atomic { implicit tx =>
                  val transpList = if( isSingle ) {
                     for( ll <- groupsView.list; idx <- indices.headOption; group <- ll.get( idx ))
                        yield document.transports( group )
                  } else {
                     None
                  }
                  transpView.list_=( transpList )
               }
         }

         transpView.guiReact {
            case ListView.SelectionChanged( indices ) =>
               val isSelected          = indices.nonEmpty
               ggDelTransp.enabled     = isSelected
               ggViewInstant.enabled   = isSelected
         }

         val splitPane = new SplitPane( Orientation.Horizontal, groupsPanel, transpPanel )

         val ggAural = Button( "Aural" ) {
            atomic { implicit tx =>
               transport.foreach { t =>
                  AuralPresentation.run( t )
               }
            }
         }

         comp = new Frame {
            title    = "Document : " + document.folder.getName
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            contents = new BorderPanel {
               add( splitPane, BorderPanel.Position.Center )
               add( ggAural, BorderPanel.Position.South )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
