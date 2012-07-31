package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Cursor, Sys}
import swing.{Button, FlowPanel, Component, BorderPanel, Frame}
import javax.swing.WindowConstants
import de.sciss.lucre.bitemp.Span
import de.sciss.synth
import synth.expr.ExprImplicits
import synth.proc.Proc

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
                                             csrPos: S#Acc )( implicit protected val cursor: Cursor[ S ])
   extends InstantGroupFrame[ S ] with ComponentHolder[ Frame ] with CursorHolder[ S ] {
      def group(     implicit tx: S#Tx ) : Document.Group[ S ]     = tx.refresh( csrPos, staleGroup )( Document.Serializers.group[ S ])
      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

      private def test() {
         atomic { implicit tx =>
            import synth._; import ugen._
            val imp  = ExprImplicits[ S ]
            import imp._
            val g    = group
            val t    = transport
            val pos  = t.time
            val span = Span( pos, pos + 44100 )
            val proc = Proc[ S ]
            proc.graph_=({
               Out.ar( 0, Pan2.ar( SinOsc.ar( "freq".kr ) * 0.2 ))
            })
            val freq = (util.Random.nextInt( 20 ) + 60).midicps
            proc.par( "freq" ).modifiableOption.foreach { bi =>
               bi.add( 0L, freq )
            }
            g.add( span, proc )
         }
      }

      def guiInit() {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         val ggTest = Button( "Test" ) {
            test()
         }
         val southPanel = new FlowPanel( transpPanel.component, ggTest )

         comp = new Frame {
            title    = "Timeline : " + staleGroup.id
            peer.setDefaultCloseOperation( WindowConstants.DO_NOTHING_ON_CLOSE )
            contents = new BorderPanel {
               add( prefusePanel.component, BorderPanel.Position.Center )
               add( southPanel, BorderPanel.Position.South )
            }
            pack()
            centerOnScreen()
            open()
         }
      }
   }
}
