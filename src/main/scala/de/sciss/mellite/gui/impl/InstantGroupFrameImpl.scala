package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Cursor, Sys}
import swing.{Component, BorderPanel, Frame}
import javax.swing.WindowConstants

object InstantGroupFrameImpl {
   def apply[ S <: Sys[ S ]]( group: Document.Group[ S ], transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : InstantGroupFrame[ S ] = {
      val prefusePanel  = InstantGroupPanel( transport )
      val transpPanel   = TransportPanel( transport )
      val view          = new Impl( prefusePanel, transpPanel, group, transport, cursor.position )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( prefusePanel: InstantGroupPanel[ S ],
                                             transpPanel: TransportPanel[ S ],
                                             staleGroup:     Document.Group[ S ],
                                             staleTransport: Document.Transport[ S ],
                                             csrPos: S#Acc )( implicit cursor: Cursor[ S ])
   extends InstantGroupFrame[ S ] {
      private var comp: Frame = _

      def group(     implicit tx: S#Tx ) : Document.Group[ S ]     = tx.refresh( csrPos, staleGroup )( Document.Serializers.group[ S ])
      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

      def component: Frame = {
         requireEDT()
         val res = comp
         if( res == null ) sys.error( "Called component before GUI was initialized" )
         res
      }

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         comp = new Frame {
            title    = "Timeline : " + staleGroup.id
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            contents = new BorderPanel {
               add( prefusePanel.component, BorderPanel.Position.Center )
               add( transpPanel.component,  BorderPanel.Position.South )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
