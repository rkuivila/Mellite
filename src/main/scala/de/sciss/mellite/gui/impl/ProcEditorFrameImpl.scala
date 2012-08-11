package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Disposable, Cursor, Sys}
import de.sciss.synth.proc.{AuralPresentation, ProcGroupX, Transport, Proc}
import swing.{Label, TextField, Orientation, SplitPane, BorderPanel, FlowPanel, Action, Button, Frame}
import de.sciss.synth.expr.{ExprImplicits, SpanLikes}
import javax.swing.WindowConstants
import de.sciss.lucre.event.Change
import java.awt.event.{WindowEvent, WindowAdapter}

object ProcEditorFrameImpl {
   def apply[ S <: Sys[ S ]]( proc: Proc[ S ])( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcEditorFrame[ S ] = {
      val view = new Impl( cursor.position, proc ) {
         protected val observer = proc.changed.reactTx { implicit tx => {
            case Proc.Rename( _, Change( _, now )) =>
               guiFromTx( name = now )
            case _ =>
         }}
      }

      val initName = proc.name.value
      guiFromTx {
         view.guiInit( initName )
      }
      view
   }

   private abstract class Impl[ S <: Sys[ S ]]( csrPos: S#Acc, staleProc: Proc[ S ])( protected implicit val cursor: Cursor[ S ])
   extends ProcEditorFrame[ S ] with ComponentHolder[ Frame ] with CursorHolder[ S ] {
      protected def observer: Disposable[ S#Tx ]

      private var ggName : TextField = _

      def name: String = {
         requireEDT()
         ggName.text
      }

      def name_=( value: String ) {
         requireEDT()
         ggName.text = value
      }

      def proc( implicit tx: S#Tx ) : Proc[ S ] = tx.refresh( csrPos, staleProc )

      def dispose()( implicit tx: S#Tx ) {
         observer.dispose()
         guiFromTx( comp.dispose() )
      }

      def guiInit( initName: String ) {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val lbName  = new Label( "Name:" )
         ggName      = new TextField( initName, 16 ) {
            action = Action( "" ) {
               val newName = text
               atomic { implicit tx =>
                  val imp  = ExprImplicits[ S ]
                  import imp._
                  proc.name_=( newName )
               }
            }
         }

         comp = new Frame {
            title = "Process : " + staleProc
            // f*** scala swing... how should this be written?
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            peer.addWindowListener( new WindowAdapter {
               override def windowClosing( e: WindowEvent ) {
                  atomic ( implicit tx => dispose() )
               }
            })

            contents = new BorderPanel {
               add( lbName, BorderPanel.Position.West )
               add( ggName, BorderPanel.Position.East )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
