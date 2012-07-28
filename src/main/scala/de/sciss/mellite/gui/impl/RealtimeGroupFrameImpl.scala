package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.Sys
import swing.{BorderPanel, Frame}
import javax.swing.WindowConstants

object RealtimeGroupFrameImpl {
   def apply[ S <: Sys[ S ]]( group: Document.Group[ S ])( implicit tx: S#Tx ) : RealtimeGroupFrame[ S ] = {
      val view = new Impl( group )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( val group: Document.Group[ S ]) extends RealtimeGroupFrame[ S ] {
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

         comp = new Frame {
            title    = "Timeline : " + group.id
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
//            contents = new BorderPanel {
//               add( groupsView.component, BorderPanel.Position.Center )
//               add( butPanel, BorderPanel.Position.South )
//            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
