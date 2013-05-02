package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.{Grapheme, Sys}
import de.sciss.lucre.stm
import Element.AudioGrapheme
import scala.swing.{Label, BoxPanel, Orientation, Swing, BorderPanel, Component}
import java.awt.Graphics2D
import de.sciss.audiowidgets.{LCDColors, LCDFont, AxisFormat, Transport, LCDPanel}
import Swing._
import de.sciss.span.Span
import de.sciss.mellite.impl.TimelineModelImpl
import de.sciss.lucre.event.Change

object AudioFileViewImpl {
  def apply[S <: Sys[S]](element: AudioGrapheme[S])(implicit tx: S#Tx): AudioFileView[S] = {
    val res = new Impl(tx.newHandle(element))
    val f   = element.entity.value // .artifact // store.resolve(element.entity.value.artifact)
    guiFromTx(res.guiInit(f))
    res
  }

  private final class Impl[S <: Sys[S]](holder: stm.Source[S#Tx, AudioGrapheme[S]])
    extends AudioFileView[S] {

    var component: Component = _

    def guiInit(snapshot: Grapheme.Value.Audio) {
      // println("AudioFileView guiInit")
      val sono = SonogramManager.acquire(snapshot.artifact)  // XXX TODO disposal
      import SonogramManager.executionContext
      //      sono.onComplete {
      //        case x => println(s"<view> $x")
      //      }
      val tlm       = new TimelineModelImpl(Span(0L, sono.inputSpec.numFrames), sono.inputSpec.sampleRate)
      val sonoView  = new AudioFileViewJ(sono, tlm)

      val timeDisp  = TimeDisplay(tlm)

      import Transport._
      val transport = Transport.makeButtonStrip(Seq(
        GoToBegin   {},
        Rewind      {},
        Stop        {},
        Play        {},
        FastForward {},
        Loop        {}
      ))
      transport.button(Stop).foreach(_.selected = true)

      val transportPane = new BoxPanel(Orientation.Horizontal) {
        contents ++= Seq(
          HStrut(4),
          new AudioFileDnD.Button(holder, snapshot, tlm),
          HGlue,
          HStrut(4),
          timeDisp.component,
          HStrut(8),
          transport,
          HStrut(4)
        )
      }

      val pane = new BorderPanel {
        layoutManager.setVgap(2)
        add(transportPane,      BorderPanel.Position.North )
        add(sonoView.component, BorderPanel.Position.Center)
      }

      component = pane
    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }
}