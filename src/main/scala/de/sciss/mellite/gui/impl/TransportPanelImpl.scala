package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Sys, Cursor}
import swing.{Action, Button, FlowPanel, Component}
import de.sciss.synth.expr.ExprImplicits

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
                                             csrPos: S#Acc )( implicit protected val cursor: Cursor[ S ])
   extends TransportPanel[ S ] with ComponentHolder[ Component ] with CursorHolder[ S ] {
      private val imp = ExprImplicits[ S ]
      import imp._

      private val playStopIcon = new PlayStopIcon()

      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val ggRTZ = Button( "|<" ) {
            atomic { implicit tx =>
               val t = transport
               t.playing_=( false )
               t.seek( 0L )
            }
         }
         val ggRewind = Button( "<<" ) {
            println( "rewind" )
         }
         lazy val ggPlayStop: Button = new Button( new Action( null ) {
            icon = playStopIcon
            def apply() {
               val res = atomic { implicit tx =>
                  val t       = transport
                  val state   = !t.playing.value
                  t.playing_=( state )
                  state
               }
               playStopIcon.state = if( res ) PlayStopIcon.Stop else PlayStopIcon.Play
               ggPlayStop.repaint()
            }
         })
         val ggForward = Button( ">>" ) {
            println( "fast forward" )
         }
         comp = new FlowPanel( ggRTZ, ggRewind, ggPlayStop, ggForward )
      }
   }
}
