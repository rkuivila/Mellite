/*
 *  TransportPanelImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.stm.{Sys, Cursor}
import swing.{BorderPanel, Orientation, BoxPanel, Label, Action, Button, FlowPanel, Component}
import de.sciss.synth.expr.ExprImplicits
import de.sciss.gui.{Transport => GUITransport, LCDPanel}
import de.sciss.synth.proc.Transport
import java.awt.Font
import java.io.{FileOutputStream, File}
import javax.swing.Box
import concurrent.stm.Txn
import java.awt.event.{ActionEvent, ActionListener}

object TransportPanelImpl {
   def apply[ S <: Sys[ S ]]( transport: Document.Transport[ S ])
                            ( implicit tx: S#Tx, cursor: Cursor[ S ]) : TransportPanel[ S ] = {
      val view    = new Impl( transport, cursor.position )
      val srk     = 1000 / transport.sampleRate
      // XXX TODO should add a more specific event that doesn't carry all the details, but just play/stop
      transport.changed.reactTx { implicit tx => {
         case Transport.Play( _ ) =>
//            println( "---play" )
//            Txn.afterCompletion( status => println( "COMPLETED WITH " + status ))( tx.peer )
            guiFromTx( view.play() )
         case Transport.Stop( tr ) =>
//            println( "---stop" )
            val mils = (tr.time * srk).toLong
            guiFromTx( view.stop( mils ))
         case Transport.Advance( _, false, time, _, _ , _ )  =>   // only needed when not playing
//            println( "---advance " + time )
            val mils = (time * srk).toLong
            guiFromTx( view.cue( mils ))
         case other =>
//            println( "---other " + other )
          // XXX TODO nasty
      }}
      val initPlaying   = transport.playing.value
      val initMillis    = (transport.time * srk).toLong
      guiFromTx {
         view.guiInit( initPlaying, initMillis )
      }
      view
   }

//   private lazy val lcdFont : Font = {
////      val f    = File.createTempFile( "tmp", ".ttf" )
//      val is   = Mellite.getClass.getResourceAsStream( "Receiptional Receipt.ttf" ) // "LCDML___.TTF"
//      require( is != null, "Font resource not found" )   // fucking Java
////      val buf  = new Array[ Byte ]( 4096 )
////      var sz   = 0
////      val os   = new FileOutputStream( f )
////      do {
////         sz = is.read( buf )
////         if( sz >= 0 ) {
////            os.write( buf, 0, sz )
////         }
////      } while( sz >= 0 )
////      is.close()
////      os.close()
////      Font.createFont( Font.TRUETYPE_FONT, f ).deriveFont( 14f )
//      val res = Font.createFont( Font.TRUETYPE_FONT, is ).deriveFont( 16f )
//      is.close()
//      res
//   }

   private final class Impl[ S <: Sys[ S ]]( staleTransport: Document.Transport[ S ],
                                             csrPos: S#Acc )( implicit protected val cursor: Cursor[ S ])
   extends TransportPanel[ S ] with ComponentHolder[ Component ] with CursorHolder[ S ] {
      private val imp = ExprImplicits[ S ]
      import imp._

      private var buttons: GUITransport.ButtonStrip = _
      private var millisVar: Long = 0L
      private var lbTime: TimeLabel = _
      private var timer: javax.swing.Timer = _

      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

//      def playing : Boolean = {
//         requireEDT()
//         buttons.button( GUITransport.Play ).map( _.selected ).getOrElse( false )
//      }

      def play() {
         requireEDT()
         playing_=( value = true )
         timer.restart()
      }

      def stop( mils: Long ) {
         requireEDT()
         timer.stop()
         playing_=( value = false )
         millis_=( mils )
      }

      private def playing_=( value: Boolean ) {
         for( ggPlay <- buttons.button( GUITransport.Play ); ggStop <- buttons.button( GUITransport.Stop )) {
            ggPlay.selected = value
            ggStop.selected = !value
         }
      }

//      def millis : Long = {
//         requireEDT()
//         millisVar
//      }

      def cue( mils: Long ) {
         requireEDT()
         millis_=( mils )
      }

      private def millis_=( value: Long ) {
//println( "----millis_(" + value + "); was " + millisVar )
         if( millisVar != value ) {
            millisVar = value
            lbTime.millis = value
         }
      }

      def guiInit( initPlaying: Boolean, initMillis: Long ) {
         requireEDT()
         require( comp == null, "Initialization called twice" )

         // use a prime number so that the visual milli update is nice
         timer = new javax.swing.Timer( 47, new ActionListener {
            def actionPerformed( e: ActionEvent ) {
               millis_=( millisVar + 47 )
            }
         })

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

//         lbTime   = new Label {
//            font  = lcdFont
//            text  = " 2:34:56.789"
//         }
         lbTime         = new TimeLabel

         val pTime = new LCDPanel {
            contents += Component.wrap( lbTime )
         }

         playing_=( initPlaying )
         millis_=( initMillis )

         val pAll = new BoxPanel( Orientation.Horizontal ) {
            contents += strip
            contents += Strut.horizontal( 8 )
            contents += pTime
//            contents += new BorderPanel {
////               contents += Component.wrap( Box.createVerticalGlue() )
////               contents += pTime
//               add( pTime, BorderPanel.Position.North )
//               add( new Label( " " ), BorderPanel.Position.Center )
////               contents += Component.wrap( Box.createVerticalGlue() )
//            }
         }

         comp     = pAll
      }
   }
}
