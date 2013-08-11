/*
 *  FrameImpl.scala
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
package realtime

import de.sciss.lucre.stm.{Source, Cursor}
import swing.{Dialog, Action, Button, FlowPanel, BorderPanel, Frame}
import javax.swing.{JComponent, WindowConstants}
import de.sciss.synth
import synth.expr.ExprImplicits
import synth.proc.{Sys, Proc}
import java.awt.event.KeyEvent
import de.sciss.span.Span
import de.sciss.desktop.KeyStrokes
import de.sciss.swingplus.DoClickAction

object FrameImpl {
  def apply[S <: Sys[S]](group: Document.Group[S], transport: Document.Transport[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupFrame[S] = {
    val prefusePanel      = InstantGroupPanel(transport)
    val transpPanel       = TransportPanel   (transport)
    implicit val groupSer = Document.Serializers.group[S]
    val groupH            = tx.newHandle(group)
    val view              = new Impl(prefusePanel, transpPanel, groupH, transport, cursor.position, group.id.toString)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](prefusePanel:   InstantGroupPanel[S],
                                        transpPanel:    TransportPanel[S],
                                        groupH:         Source[S#Tx, Document.Group[S]],
                                        val transport:  Document.Transport[S],
                                        csrPos:         S#Acc,
                                        name:           String)
                                       (implicit protected val cursor: Cursor[S])
    extends InstantGroupFrame[S] with ComponentHolder[Frame] with CursorHolder[S] {
    def group(implicit tx: S#Tx): Document.Group[S] = groupH() // tx.refresh( csrPos, staleGroup )( Document.Serializers.group[ S ])
    //      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

    private def newProc(): Unit =
      Dialog.showInput(parent = prefusePanel.component, message = "Name for new process:", title = "New Process",
        messageType = Dialog.Message.Question, initial = "Unnamed").foreach(newProc)

    private def newProc(name: String): Unit =
      atomic { implicit tx =>
        // import synth._; import ugen._
        val imp = ExprImplicits[S]
        import imp._
        val g = group
        val t = transport
        val pos = t.time
        val span = Span(pos, pos + 44100)
        val proc = Proc[S]
        // proc.name_=(name)
        //            proc.graph_=({
        //               Out.ar( 0, Pan2.ar( SinOsc.ar( "freq".kr ) * 0.2 ))
        //            })
        //            val freq = (util.Random.nextInt( 20 ) + 60).midicps
        //            proc.par( "freq" ).modifiableOption.foreach { bi =>
        //               bi.add( 0L, freq )
        //            }
        g.add(span, proc)
      }

    def guiInit(): Unit = {
      requireEDT()
      require(comp == null, "Initialization called twice")

      import KeyStrokes._

      val ggTest = new Button {
        private val actionKey = "de.sciss.mellite.NewProc"
        peer.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(menu1 + KeyEvent.VK_1, actionKey)
        peer.getActionMap.put(actionKey, DoClickAction(this).peer)
        focusable = false
        action = Action("New Proc") {
          newProc()
        }
      }

      val southPanel = new FlowPanel(transpPanel.component, ggTest)

      comp = new Frame {
        title = "Timeline : " + name // staleGroup.id
        peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
        contents = new BorderPanel {
          add(prefusePanel.component, BorderPanel.Position.Center)
          add(southPanel, BorderPanel.Position.South)
        }
        pack()
        centerOnScreen()
        open()
      }
    }
  }
}
