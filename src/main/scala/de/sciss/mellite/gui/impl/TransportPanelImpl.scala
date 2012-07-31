package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Sys, Cursor}
import swing.{Button, FlowPanel, Component}

object TransportPanelImpl {
   def apply[ S <: Sys[ S ]]( transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : TransportPanel[ S ] = {
      val view    = new Impl( transport, cursor.position )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( staleTransport: Document.Transport[ S ],
                                             csrPos: S#Acc )( implicit cursor: Cursor[ S ])
   extends TransportPanel[ S ] with ComponentHolder[ Component ] {

      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val ggRTZ = Button( "|<" ) {
            println( "return to zero" )
         }
         val ggRewind = Button( "<<" ) {
            println( "rewind" )
         }
         val ggStop = Button( "\u2610" ) {
            println( "stop" )
         }
         val ggPlay = Button( ">" ) {  // "\u25B7"
            println( "play" )
         }
         val ggForward = Button( ">>" ) {
            println( "fast forward" )
         }
         comp = new FlowPanel( ggRTZ, ggRewind, ggStop, ggPlay, ggForward )
      }
   }
}
