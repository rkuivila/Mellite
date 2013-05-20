package de.sciss
package mellite
package gui
package impl

import lucre.{stm, event => evt}
import synth.proc
import proc.{AuralPresentation, AuralSystem, Sys, ProcGroup}
import java.awt.event.{KeyEvent, ActionListener, ActionEvent}
import scala.swing.{Swing, Action, Orientation, BoxPanel, Component}
import audiowidgets.Transport
import span.Span
import desktop.{FocusType, KeyStrokes}
import stm.Disposable
import Swing._

object TransportViewImpl {
  def apply[S <: Sys[S], I <: stm.Sys[I]](group: ProcGroup[S], sampleRate: Double, timelineModel: TimelineModel)
                                         (implicit tx: S#Tx, cursor: stm.Cursor[S],
                                          bridge: S#Tx => I#Tx, aural: AuralSystem): TransportView[S] = {
    val transp    = proc.Transport[S, I](group, sampleRate)
    val auralPres = AuralPresentation.runTx[S](transp, aural)

    new Impl[S] {
      val transport         = transp
      val auralPresentation = auralPres
      val _cursor           = cursor
      val _timelineModel    = timelineModel
      val samplesPerMilli   = 0.001 * transport.sampleRate
      val observer          = transp.react { implicit tx => {
        case proc.Transport.Play(t, time) => startedPlaying(time)
        case proc.Transport.Stop(t, time) => stoppedPlaying(time)
        case _ => // proc.Transport.Advance(t, time, isSeek, isPlaying, _, _, _) =>
      }}
      guiFromTx(guiInit())
    }
  }

  private abstract class Impl[S <: Sys[S]]
    extends TransportView[S] with ComponentHolder[Component] {

    protected val _cursor       : stm.Cursor[S]
    protected def _timelineModel: TimelineModel
    protected def observer      : Disposable[S#Tx]
    protected def samplesPerMilli: Double

    import _cursor.step

    private var timerFrame  = 0L
    private var timerSys    = 0L
    // private val srm         = 0.001 * transport.sampleRate
    private val timer       = new javax.swing.Timer(31, new ActionListener {
      def actionPerformed(e: ActionEvent) {
        val elapsed             = ((System.currentTimeMillis() - timerSys) * samplesPerMilli).toLong
        _timelineModel.position  = timerFrame + elapsed
      }
    })

    private var transportStrip: Component with Transport.ButtonStrip = _

    def dispose()(implicit tx: S#Tx) {
      timer.stop()  // save to call multiple times
      observer.dispose()
    }

    // ---- transport ----

    def startedPlaying(time: Long)(implicit tx: S#Tx) {
      guiFromTx {
        timer.stop()
        timerFrame  = time
        timerSys    = System.currentTimeMillis()
        timer.start()
        transportStrip.button(Transport.Play).foreach(_.selected = true )
        transportStrip.button(Transport.Stop).foreach(_.selected = false)
      }
    }

    def stoppedPlaying(time: Long)(implicit tx: S#Tx) {
      guiFromTx {
        timer.stop()
        _timelineModel.position = time // XXX TODO if Cursor follows Playhead
        transportStrip.button(Transport.Play).foreach(_.selected = false)
        transportStrip.button(Transport.Stop).foreach(_.selected = true )
      }
    }

    private def rtz() {
      stop()
      val start = _timelineModel.bounds.start
      _timelineModel.position  = start
      _timelineModel.visible   = Span(start, start + _timelineModel.visible.length)
    }

    private def rewind() {

    }

    private def playOrStop() {
      step { implicit tx =>
        if (transport.isPlaying) transport.stop() else {
          transport.seek(_timelineModel.position)
          transport.play()
        }
      }
    }

    private def stop() {
      step { implicit tx => transport.stop() }
    }

    private def play() {
      step { implicit tx =>
        transport.stop()
        transport.seek(_timelineModel.position)
        transport.play()
      }
    }

    private def ffwd() {

    }

    def guiInit() {
      val timeDisp    = TimeDisplay(_timelineModel)

      import Transport.{Action => _, _}
      transportStrip = Transport.makeButtonStrip(Seq(
        GoToBegin   { rtz()    },
        Rewind      { rewind() },
        Stop        { stop()   },
        Play        { play()   },
        FastForward { ffwd()   },
        Loop        {}
      ))
      transportStrip.button(Stop).foreach(_.selected = true)

      import KeyEvent._
      import KeyStrokes._

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          timeDisp.component,
          HStrut(8),
          transportStrip
        )
      }

      import desktop.Implicits._

      transportPane.addAction("playstop", focus = FocusType.Window, action = new Action("playstop") {
        accelerator = Some(plain + VK_SPACE)
        def apply() {
          playOrStop()
        }
      })
      transportPane.addAction("rtz", focus = FocusType.Window, action = new Action("rtz") {
        accelerator = Some(plain + VK_ENTER)
        def apply() {
          transportStrip.button(GoToBegin).foreach(_.doClick())
        }
      })

      comp = transportPane
    }
  }
}