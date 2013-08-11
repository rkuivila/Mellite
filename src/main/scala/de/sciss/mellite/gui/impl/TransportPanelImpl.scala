/*
 *  TransportPanelImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.lucre.stm.Cursor
import scala.swing.{Swing, Orientation, BoxPanel, Component}
import de.sciss.audiowidgets.{Transport => GUITransport, LCDPanel}
import de.sciss.synth.proc.Sys
import java.awt.event.{ActionEvent, ActionListener}
import javax.swing.event.{ChangeEvent, ChangeListener}
import Swing._
import de.sciss.mellite.gui.impl.component.TimeLabel

object TransportPanelImpl {
  def apply[S <: Sys[S]](transport: Document.Transport[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): TransportPanel[S] = {
    val view  = new Impl(transport, cursor.position)
    val srk   = 1000 / transport.sampleRate
    // XXX TODO should add a more specific event that doesn't carry all the details, but just play/stop
    //      transport.reactTx { implicit tx => {
    //         case Transport.Play( _ ) =>
    ////            println( "---play" )
    ////            Txn.afterCompletion( status => println( "COMPLETED WITH " + status ))( tx.peer )
    //            guiFromTx( view.play() )
    //         case Transport.Stop( tr ) =>
    ////            println( "---stop" )
    //            val mils = (tr.time * srk).toLong
    //            guiFromTx( view.stop( mils ))
    //         case Transport.Advance( _, false, time, _, _ , _ )  =>   // only needed when not playing
    ////            println( "---advance " + time )
    //            val mils = (time * srk).toLong
    //            guiFromTx( view.cue( mils ))
    //         case other =>
    ////            println( "---other " + other )
    //          // XXX TODO nasty
    //      }}
    val initPlaying = transport.isPlaying // .playing.value
    val initMillis = (transport.time * srk).toLong
    guiFromTx {
      view.guiInit(initPlaying, initMillis)
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

  private final class Impl[S <: Sys[S]](val transport: Document.Transport[S],
                                        csrPos: S#Acc)(implicit protected val cursor: Cursor[S])
    extends TransportPanel[S] with ComponentHolder[Component] with CursorHolder[S] {

    import GUITransport.{GoToBegin, Rewind, FastForward, Stop, Play}

    private var buttons: GUITransport.ButtonStrip = _
    private var millisVar: Long = 0L
    private var lbTime: TimeLabel = _
    private var playTimer: javax.swing.Timer = _
    private var cueTimer: javax.swing.Timer = _
    private var playingVar = false
    private var cueDirection = 1

    //      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

    //      def playing : Boolean = {
    //         requireEDT()
    //         buttons.button( GUITransport.Play ).map( _.selected ).getOrElse( false )
    //      }

    def play(): Unit = {
      requireEDT()
      playing_=(value = true)
      playTimer.restart()
    }

    def stop(mils: Long): Unit = {
      requireEDT()
      playTimer.stop()
      playing_=(value = false)
      millis_=(mils)
    }

    private def playing: Boolean = playingVar
    private def playing_=(value: Boolean): Unit = {
      playingVar = value
      cueTimer.stop()
      for (ggPlay <- buttons.button(Play); ggStop <- buttons.button(Stop)) {
        ggPlay.selected = value
        ggStop.selected = !value
      }
    }

    //      def millis : Long = {
    //         requireEDT()
    //         millisVar
    //      }

    def cue(mils: Long): Unit = {
      requireEDT()
      millis_=(mils)
    }

    private def millis_=(value: Long): Unit = {
      //println( "----millis_(" + value + "); was " + millisVar )
      if (millisVar != value) {
        millisVar = value
        lbTime.millis = value
      }
    }

    def guiInit(initPlaying: Boolean, initMillis: Long): Unit = {
       requireEDT()
       require(comp == null, "Initialization called twice")

       // use a prime number so that the visual milli update is nice
       playTimer = new javax.swing.Timer(47, new ActionListener {
         def actionPerformed(e: ActionEvent): Unit =
           millis_=(millisVar + 47)
       })

       cueTimer = new javax.swing.Timer(63, new ActionListener {
         def actionPerformed(e: ActionEvent): Unit =
           if (!playing) atomic { implicit tx =>
             val t = transport
             t.seek(t.time + (t.sampleRate * 0.25 * cueDirection).toLong)
           }
       })

       val actionRTZ = GoToBegin {
         atomic { implicit tx =>
           val t = transport
           t.stop() // t.playing_=( false )
           t.seek(0L)
         }
       }

       val actionRewind = Rewind()

         val actionPlay = Play {
            atomic { implicit tx =>
               transport.play() // .playing_=( true )
            }
         }

         val actionStop = Stop {
            atomic { implicit tx =>
               transport.stop() // .playing_=( false )
            }
         }

         val actionFFwd = FastForward()

       val actions  = Seq(actionRTZ, actionRewind, actionPlay, actionStop, actionFFwd)
       val strip    = GUITransport.makeButtonStrip(actions)

       Seq(Rewind -> -1, FastForward -> 1).foreach {
         case (act, dir) => strip.button(act).foreach { b =>
           val m = b.peer.getModel
           m.addChangeListener(new ChangeListener {
             var pressed = false

             def stateChanged(e: ChangeEvent): Unit = {
               val p = m.isPressed
               if (p != pressed) {
                 pressed = p
                 if (p) {
                   //println( "-restart" )
                   cueDirection = dir
                   cueTimer.restart()
                 } else {
                   //println( "-stop" )
                   cueTimer.stop()
                 }
               }
             }
           })
         }
       }

       buttons = strip

       //         lbTime   = new Label {
       //            font  = lcdFont
       //            text  = " 2:34:56.789"
       //         }
       lbTime = new TimeLabel

       val pTime = new LCDPanel {
         contents += Component.wrap(lbTime)
       }

       playing_=(initPlaying)
       millis_=(initMillis)

       val pAll = new BoxPanel(Orientation.Horizontal) {
         contents += strip
         contents += HStrut(8)
         contents += pTime
         //            contents += new BorderPanel {
         ////               contents += Component.wrap( Box.createVerticalGlue() )
         ////               contents += pTime
         //               add( pTime, BorderPanel.Position.North )
         //               add( new Label( " " ), BorderPanel.Position.Center )
         ////               contents += Component.wrap( Box.createVerticalGlue() )
         //            }
       }

       comp = pAll
     }
   }
}
