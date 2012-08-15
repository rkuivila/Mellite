package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Disposable, Cursor, Sys}
import de.sciss.synth.proc.{ProcGraph, Proc}
import swing.{Button, FlowPanel, Component, Label, TextField, BorderPanel, Action, Frame}
import de.sciss.synth.expr.ExprImplicits
import javax.swing.WindowConstants
import de.sciss.lucre.event.Change
import java.awt.event.{WindowEvent, WindowAdapter}
import de.sciss.scalainterpreter.{Interpreter, CodePane}
import java.awt.Dimension
import de.sciss.synth.SynthGraph

object ProcEditorFrameImpl {
   def apply[ S <: Sys[ S ]]( proc: Proc[ S ])( implicit tx: S#Tx, cursor: Cursor[ S ]) : ProcEditorFrame[ S ] = {
      val view = new Impl( cursor.position, proc ) {
         protected val observer = proc.changed.reactTx { implicit tx => {
            case Proc.Rename( _, Change( _, now )) =>
               guiFromTx( name = now )
            case _ =>
         }}
      }

      val initName         = proc.name.value
      val initGraphSource  = proc.graph.sourceCode
      guiFromTx {
         view.guiInit( initName, initGraphSource )
      }
      view
   }

   private abstract class Impl[ S <: Sys[ S ]]( csrPos: S#Acc, staleProc: Proc[ S ])( protected implicit val cursor: Cursor[ S ])
   extends ProcEditorFrame[ S ] with ComponentHolder[ Frame ] with CursorHolder[ S ] {
      protected def observer: Disposable[ S#Tx ]

      private var ggName : TextField = _
      private var ggSource : CodePane = _
      private var intOpt = Option.empty[ Interpreter ]

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

      def guiInit( initName: String, initGraphSource: String ) {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val cCfg     = CodePane.Config()
         cCfg.text    = initGraphSource
//         cCfg.keyMap += KeyStroke.getKeyStroke( KeyEvent.VK_ENTER, InputEvent.SHIFT_MASK ) -> { () =>
//            intOpt.foreach { in =>
//               println( in.interpret( ggSource.editor.getText ))
//            }
//         }
         ggSource     = CodePane( cCfg )
         ggSource.editor.setEnabled( false )
         ggSource.editor.setPreferredSize( new Dimension( 300, 300 ))
         InterpreterSingleton { in =>
            execInGUI {
               intOpt = Some( in )
               ggSource.editor.setEnabled( true )
            }
         }

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

         val lbStatus = new Label
         lbStatus.preferredSize = new Dimension( 200, lbStatus.preferredSize.height )

         val ggCommit = Button( "Commit" ) {
            intOpt.foreach { in =>
               val code = ggSource.editor.getText
               if( code != "" ) {
                  val wrapped = /* InterpreterSingleton.wrap( */ "SynthGraph {\n" + code + "\n}" /* ) */
                  val intRes = in.interpret( wrapped )
//println( "Interpreter done " + intRes )
                  intRes match {
                     case Interpreter.Success( name, value ) =>
//                        val value = InterpreterSingleton.Result.value
//println( "Success " + name + " value is " + value )
                        value match {
                           case sg: SynthGraph =>
//println( "Success " + name + " is a graph" )
                              lbStatus.text = "Ok."
                              atomic { implicit tx =>
                                 proc.graph_=( ProcGraph( sg, code ))
                              }
                           case _ =>
                              lbStatus.text = "! Invalid result: " + value + " !"
                        }
                     case Interpreter.Error( _ ) =>
                        lbStatus.text = "! Error !"
                     case Interpreter.Incomplete =>
                        lbStatus.text = "! Code incomplete !"
                  }
               }
            }
         }

         val topPanel   = new FlowPanel( lbName, ggName )
         val botPanel   = new FlowPanel( ggCommit, lbStatus )

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
               import BorderPanel.Position._
               add( topPanel, North )
               add( Component.wrap( ggSource.component ), Center )
               add( botPanel, South )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
