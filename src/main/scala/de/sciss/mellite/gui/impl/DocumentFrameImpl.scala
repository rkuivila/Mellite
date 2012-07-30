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

import swing.{FlowPanel, Action, Button, BorderPanel, Frame}
import de.sciss.lucre.stm.Sys
import javax.swing.WindowConstants
import de.sciss.synth.proc.ProcGroupX
import de.sciss.synth.expr.SpanLikes

object DocumentFrameImpl {
   def apply[ S <: Sys[ S ]]( doc: Document[ S ])( implicit tx: S#Tx ) : DocumentFrame[ S ] = {
      implicit val csr = doc.cursor
      implicit val groupsSer = Document.Serializers.groups[ S ]
      val groupsView = ListView( doc.groups )( g => "Timeline " + g.id )
      val view = new Impl( doc, groupsView )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( val document: Document[ S ],
                                             groupsView: ListView[ S, Document.Group[ S ], Document.GroupUpdate[ S ]])
   extends DocumentFrame[ S ] {
      private var comp: Frame = _

      def component: Frame = {
         requireEDT()
         val res = comp
         if( res == null ) sys.error( "Called component before GUI was initialized" )
         res
      }

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val ggAdd = Button( "+" ) {
            document.cursor.step { implicit tx =>
               implicit val spans = SpanLikes
               val group = ProcGroupX.Modifiable[ S ]
               document.groups.addLast( group )
            }
         }

         val ggDel = new Button( Action( "\u2212" ) {
            val indices = groupsView.guiSelection
            if( indices.nonEmpty ) document.cursor.step { implicit tx =>
               val g    = document.groups
               val sz   = g.size
               val ind1 = indices.filter( _ < sz ).sortBy( -_ )
               ind1.foreach( g.removeAt )
            }
         }) {
            enabled = false
         }

         val ggView = new Button( Action( "View" ) {

         }) {
            enabled = false
         }

         val butPanel = new FlowPanel( ggAdd, ggDel, ggView )

         groupsView.guiReact {
            case ListView.SelectionChanged( indices ) =>
//               println( "SELECTION " + indices )
               val isSelected = indices.nonEmpty
               ggDel.enabled  = isSelected
               ggView.enabled = isSelected
         }

         comp = new Frame {
            title    = "Document : " + document.folder.getName
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            contents = new BorderPanel {
               add( groupsView.component, BorderPanel.Position.Center )
               add( butPanel, BorderPanel.Position.South )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
