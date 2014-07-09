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
import scala.swing.{Component, Dialog, Action, Button, FlowPanel, BorderPanel}
import de.sciss.synth.proc.{Timeline, Transport, ProcGroupElem, Obj, ProcGroup, ExprImplicits, Proc}
import de.sciss.span.Span
import de.sciss.desktop
import de.sciss.desktop.{FocusType, KeyStrokes}
import de.sciss.swingplus.DoClickAction
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing._
import scala.swing.event.Key
import de.sciss.synth.proc
import proc.Implicits._
import de.sciss.audiowidgets.impl.TimelineModelImpl

object FrameImpl {
  def apply[S <: Sys[S]](document: Workspace[S], obj: Obj.T[S, ProcGroupElem] /*, transport: Document.Transport[S] */)
                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupFrame[S] = {
    val sampleRate        = Timeline.SampleRate
    val group             = obj.elem.peer
    import document.inMemoryBridge
    val transport: Transport[S] = ??? // = proc.Transpor [S, document.I](group, sampleRate = sampleRate)
    val prefusePanel      = InstantGroupPanel(document, transport)
    // note: the transport only reads and updates the position, as well as reading span start for return-to-zero
    val tlm               = new TimelineModelImpl(Span(0L, (sampleRate * 60 * 60).toLong), sampleRate)
    val transportPanel    = TransportView(transport, tlm, hasMillis = false, hasLoop = false)
    import ProcGroup.serializer
    val groupH            = tx.newHandle(group)
    val name              = obj.attr.name
    val view              = new Impl(prefusePanel, transportPanel, groupH, transport, name = name)
    deferTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val view      : InstantGroupPanel[S],
                                        transportPanel: TransportView[S],
                                        groupH        : Source[S#Tx, ProcGroup[S]], // Document.Group[S]],
                                        val transport : Transport[S],
                                        name          : String)
                                       (implicit protected val cursor: Cursor[S])
    extends InstantGroupFrame[S] with WindowHolder[desktop.Window] with CursorHolder[S] {

    def group(implicit tx: S#Tx): ProcGroup[S] = groupH() // tx.refresh( csrPos, staleGroup )( Document.Serializers.group[ S ])
    //      def transport( implicit tx: S#Tx ) : Document.Transport[ S ] = tx.refresh( csrPos, staleTransport )

    private def newProc(): Unit =
      Dialog.showInput(parent = view.component, message = "Name for new process:", title = "New Process",
        messageType = Dialog.Message.Question, initial = "Unnamed").foreach(newProc)

    def contents: InstantGroupPanel[S] = view

    def component: Component = view.component

    private def newProc(name: String): Unit =
      atomic { implicit tx =>
        // import synth._; import ugen._
        val imp = ExprImplicits[S]
        import imp._
        group.modifiableOption.foreach { g =>
          val t = transport
          val pos = t.position
          val span = Span(pos, pos + Timeline.SampleRate.toLong)
          val proc = Proc[S]
          ??? // g.add(span, proc)
        }
      }

    private def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      deferTx {
        // DocumentViewHandler.instance.remove(this)
        window.dispose()
      }
    }

    private def disposeData()(implicit tx: S#Tx): Unit = {
      view.dispose()
      transport.dispose()
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

//      val f = new WindowImpl {
//        title = s"$name : Real-time" // staleGroup.id
//        // peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
//        contents = new BorderPanel {
//          add(view.component, BorderPanel.Position.Center)
//          add(southPanel, BorderPanel.Position.South)
//        }
//
//        reactions += {
//          case desktop.Window.Closing(_) => frameClosing()
//        }
//
//        pack()
//        // centerOnScreen()
//        GUI.centerOnScreen(this)
//        front()
//      }

      window = ??? // f
    }
  }
}
