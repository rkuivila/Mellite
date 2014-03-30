/*
 *  FrameImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
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
import javax.swing.WindowConstants
import de.sciss.synth.proc.{ExprImplicits, Proc}
import de.sciss.span.Span
import de.sciss.desktop
import de.sciss.desktop.{FocusType, KeyStrokes}
import de.sciss.swingplus.DoClickAction
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import scala.swing.event.Key

object FrameImpl {
  def apply[S <: Sys[S]](group: Document.Group[S], transport: Document.Transport[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupFrame[S] = {
    val prefusePanel      = InstantGroupPanel(transport)
    val transportPanel    = TransportPanel   (transport)
    implicit val groupSer = Document.Serializers.group[S]
    val groupH            = tx.newHandle(group)
    val view              = new Impl(prefusePanel, transportPanel, groupH, transport, cursor.position, group.id.toString)
    deferTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](prefusePanel:   InstantGroupPanel[S],
                                        transportPanel: TransportPanel[S],
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
        g.add(span, proc)
      }

    def guiInit(): Unit = {
      import KeyStrokes._
      import desktop.Implicits._

      val ggTest = new Button {
        private val actionKey = "de.sciss.mellite.NewProc"
        this.addAction(actionKey, new DoClickAction(this) {
          accelerator = Some(menu1 + Key.Key1)
        }, FocusType.Window)
        focusable = false
        action = Action("New Proc") {
          newProc()
        }
      }

      val southPanel = new FlowPanel(transportPanel.component, ggTest)

      component = new Frame {
        title = s"Timeline : $name" // staleGroup.id
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
