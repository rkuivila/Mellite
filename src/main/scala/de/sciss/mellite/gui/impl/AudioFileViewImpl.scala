package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import de.sciss.lucre.stm
import Element.AudioGrapheme
import scala.swing.{Label, BoxPanel, Orientation, Swing, BorderPanel, Component}
import de.sciss.sonogram
import java.io.File
import java.awt.Graphics2D
import de.sciss.audiowidgets.{LCDColors, LCDFont, AxisFormat, Transport, LCDPanel}
import Swing._
import de.sciss.span.Span
import de.sciss.mellite.impl.TimelineModelImpl

object AudioFileViewImpl {
  private lazy val manager = {
    val cfg       = sonogram.OverviewManager.Config()
    val folder    = new File(new File(sys.props("user.home"), "mellite"), "cache")
    folder.mkdirs()
    val sizeLimit = 2L << 10 << 10 << 10  // 2 GB
    cfg.caching = Some(sonogram.OverviewManager.Caching(folder, sizeLimit))
    sonogram.OverviewManager(cfg)
  }

  def apply[S <: Sys[S]](element: AudioGrapheme[S])(implicit tx: S#Tx): AudioFileView[S] = {
    val res = new Impl(tx.newHandle(element))
    val f   = element.entity.value.artifact // store.resolve(element.entity.value.artifact)
    guiFromTx(res.guiInit(f))
    res
  }

  private final class Impl[S <: Sys[S]](holder: stm.Source[S#Tx, AudioGrapheme[S]])
    extends AudioFileView[S] {

    var component: Component = _

    def guiInit(f: File) {
      // println("AudioFileView guiInit")
      val sono      = manager.acquire(sonogram.OverviewManager.Job(f))
      implicit val exec = manager.config.executionContext
      //      sono.onComplete {
      //        case x => println(s"<view> $x")
      //      }
      val tlm       = new TimelineModelImpl(Span(0L, sono.inputSpec.numFrames), sono.inputSpec.sampleRate)
      val sonoView  = new AudioFileViewJ(sono, tlm)

      val lcdFormat = AxisFormat.Time(hours = true, millis = true)
      val lcd       = new Label {
        override protected def paintComponent(g2: Graphics2D) {
          val atOrig  = g2.getTransform
          try {
            // stupid lcd font has wrong ascent
            g2.translate(0, 4)
            // g2.setColor(java.awt.Color.red)
            // g2.fillRect(0, 0, 100, 100)
            super.paintComponent(g2)
          } finally {
            g2.setTransform(atOrig)
          }
        }

        font        = LCDFont().deriveFont(11f)
        foreground  = LCDColors.defaultFg
        text        = lcdFormat.format(0.0, decimals = 3, pad = 12)

        maximumSize = preferredSize
        minimumSize = preferredSize
      }
      //      lcd.setMinimumSize(lcd.getPreferredSize)
      //      lcd.setMaximumSize(lcd.getPreferredSize)
      val lcdFrame  = new LCDPanel {
        maximumSize = preferredSize
        minimumSize = preferredSize
        contents += lcd
      }
      val lcdPane = new BoxPanel(Orientation.Vertical) {
        contents += VGlue
        contents += lcdFrame
        contents += VGlue
      }

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
          HGlue,
          HStrut(4),
          lcdPane,
          HStrut(8),
          transport,
          HStrut(4)
        )
      }

      val pane = new BorderPanel {
        layoutManager.setVgap(2)
        add(transportPane, BorderPanel.Position.North )
        add(sonoView     , BorderPanel.Position.Center)
      }

      component = pane
    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }
}