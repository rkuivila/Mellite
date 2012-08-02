package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Sys, Cursor}
import swing.{Action, Button, FlowPanel, Component}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.gui.{Transport => GUITransport}

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

      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

      def guiInit() {
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
         comp = GUITransport.makeButtonStrip( actions )
      }
   }
}
