package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Sys, Cursor}
import swing.{Action, Button, FlowPanel, Component}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.gui.{Transport => GUITransport}
import de.sciss.synth.proc.Transport

object TransportPanelImpl {
   def apply[ S <: Sys[ S ]]( transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : TransportPanel[ S ] = {
      val view    = new Impl( transport, cursor.position )
      // XXX TODO should add a more specific event that doesn't carry all the details, but just play/stop
      transport.changed.react {
         case Transport.Play( _ ) => guiFromTx( view.playing = true  )
         case Transport.Stop( _ ) => guiFromTx( view.playing = false )
         case _ =>  // XXX TODO nasty
      }
      val wasPlaying = transport.playing.value
      guiFromTx {
         view.guiInit( wasPlaying )
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( staleTransport: Document.Transport[ S ],
                                             csrPos: S#Acc )( implicit protected val cursor: Cursor[ S ])
   extends TransportPanel[ S ] with ComponentHolder[ Component ] with CursorHolder[ S ] {
      private val imp = ExprImplicits[ S ]
      import imp._

      private var buttons: GUITransport.ButtonStrip = _

      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

      def playing : Boolean = {
         requireEDT()
         buttons.button( GUITransport.Play ).map( _.selected ).getOrElse( false )
      }
      def playing_=( value: Boolean ) {
         requireEDT()
         for( ggPlay <- buttons.button( GUITransport.Play ); ggStop <- buttons.button( GUITransport.Stop )) {
            ggPlay.selected = value
            ggStop.selected = !value
         }
      }

      def guiInit( initPlaying: Boolean ) {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val actionRTZ = GUITransport.GoToBegin {
            atomic { implicit tx =>
               val t = transport
               t.playing_=( false )
               t.seek( 0L )
            }
         }

         val actionRewind = GUITransport.Rewind {
            println( "rewind" )
         }

         val actionPlay = GUITransport.Play {
            atomic { implicit tx =>
               transport.playing_=( true )
            }
         }

         val actionStop = GUITransport.Stop {
            atomic { implicit tx =>
               transport.playing_=( false )
            }
         }

         val actionFFwd = GUITransport.FastForward {
            println( "fast forward" )
         }

         val actions = Seq( actionRTZ, actionRewind, actionPlay, actionStop, actionFFwd )
         val strip   = GUITransport.makeButtonStrip( actions )
         buttons  = strip

         playing = initPlaying

         comp     = strip
      }
   }
}
