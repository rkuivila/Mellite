package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import de.sciss.lucre.stm
import Element.AudioGrapheme
import swing.Component
import de.sciss.sonogram
import java.io.File
import de.sciss.audiowidgets.j.{Transport, LCDPanel}
import java.awt.{Dimension, Graphics2D, Graphics, BorderLayout}
import javax.swing.{JLabel, Box, JPanel}
import de.sciss.audiowidgets.{LCDColors, LCDFont, AxisFormat}

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
      val sonoView  = new AudioFileViewJ(sono)

      val lcdFrame  = new LCDPanel {
        override def getMaximumSize = getPreferredSize
        override def getMinimumSize = getPreferredSize
      }
      val lcd       = new JLabel {
        override def getMaximumSize = getPreferredSize
        override def getMinimumSize = getPreferredSize

        override protected def paintComponent(g: Graphics) {
          val g2      = g.asInstanceOf[Graphics2D]
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
      }
      val lcdFormat = AxisFormat.Time(hours = true, millis = true)
      lcd.setFont(LCDFont().deriveFont(11f))
      lcd.setForeground(LCDColors.defaultFg)
      lcd.setText(lcdFormat.format(0.0, decimals = 3, pad = 12))
      //      lcd.setMinimumSize(lcd.getPreferredSize)
      //      lcd.setMaximumSize(lcd.getPreferredSize)
      lcdFrame.add(lcd, BorderLayout.CENTER)
      val lcdPane   = Box.createVerticalBox()
      lcdPane.add(Box.createVerticalGlue())
      lcdPane.add(lcdFrame)
      lcdPane.add(Box.createVerticalGlue())

      import Transport._
      val transport = Transport.makeButtonStrip(Seq(
        GoToBegin   {},
        Rewind      {},
        Stop        {},
        Play        {},
        FastForward {},
        Loop        {}
      ))
      transport.button(Stop).foreach(_.setSelected(true))

      val transportPane = Box.createHorizontalBox()
      transportPane.add(Box.createHorizontalGlue())
      transportPane.add(Box.createHorizontalStrut(4))
      transportPane.add(lcdPane)
      transportPane.add(Box.createHorizontalStrut(8))
      transportPane.add(transport)
      transportPane.add(Box.createHorizontalStrut(4))

      val pane = new JPanel(new BorderLayout(0, 2))
      pane.add(transportPane, BorderLayout.NORTH )
      pane.add(sonoView     , BorderLayout.CENTER)

      component = Component.wrap(pane)
    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }
}