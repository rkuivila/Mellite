package de.sciss.mellite
package gui
package impl

import swing.{Button, BorderPanel, Frame}
import de.sciss.lucre.stm.Sys
import javax.swing.WindowConstants
import de.sciss.lucre.expr.BiGroup
import de.sciss.synth.proc.Proc
import de.sciss.synth.expr.SpanLikes

object DocumentFrameImpl {
   def apply[ S <: Sys[ S ]]( doc: Document[ S ])( implicit tx: S#Tx ) : DocumentFrame[ S ] = {
      val groupsView = ListView( doc.groups )( _.toString )
      val view = new Impl( doc, groupsView )
      guiFromTx {
         view.guiInit()
      }
      view
   }

   private final class Impl[ S <: Sys[ S ]]( val document: Document[ S ], groupsView: ListView[ S ])
   extends DocumentFrame[ S ] {
      private var comp: Frame = _

      def component: Frame = {
         val res = comp
         if( res == null ) sys.error( "Called component before GUI was initialized" )
         res
      }

      def guiInit() {
         val testBut = Button( "Test Add" ) {
            document.cursor.step { implicit tx =>
               implicit val spans = SpanLikes
               val group = BiGroup.newVar[ S, Proc[ S ], Proc.Update[ S ]]( _.changed )
               document.groups.addLast( group )
            }
         }

         comp = new Frame {
            title    = "Document : " + document.folder.getName
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            contents = new BorderPanel {
               add( groupsView.component, BorderPanel.Position.Center )
               add( testBut, BorderPanel.Position.South )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
