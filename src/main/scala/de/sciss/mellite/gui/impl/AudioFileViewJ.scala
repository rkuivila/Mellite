package de.sciss.mellite
package gui
package impl

import de.sciss.sonogram
import javax.swing.{JScrollBar, Box, JPanel, SwingConstants, JComponent}
import java.awt.image.BufferedImage
import java.awt.{BorderLayout, Color, Graphics, Graphics2D, Rectangle, TexturePaint}
import de.sciss.audiowidgets.j.{PeakMeterBar, Axis}
import de.sciss.audiowidgets.AxisFormat
import scala.util.{Success, Failure}
import scala.concurrent.ExecutionContext

final class AudioFileViewJ(sono: sonogram.Overview)
  extends JPanel {

  private val numChannels = sono.inputSpec.numChannels
  private val numFrames   = sono.inputSpec.numFrames
  private val sampleRate  = sono.inputSpec.sampleRate
  private val dur         = numFrames / sampleRate
  // private val minFreq     = sono.config.sonogram.minFreq
  // private val maxFreq     = sono.config.sonogram.maxFreq

  private val timeAxis  = {
    val res     = new Axis(SwingConstants.HORIZONTAL)
    res.format  = AxisFormat.Time(hours = dur >= 3600.0, millis = true)
    res.maximum = dur
    res
  }

  // XXX TODO: Axis lost its logarithmic scale a while back. Need to reimplement
  //  private val freqAxes  = Vector.fill(numChannels) {
  //    val res       = new Axis(SwingConstants.VERTICAL)
  //    res.minimum   = minFreq
  //    res.maximum   = maxFreq
  //  }

  private val meters  = Vector.fill(numChannels) {
    val res   = new PeakMeterBar(SwingConstants.VERTICAL)
    res.ticks = 50
    res
  }

  private object SonoView extends JComponent with sonogram.PaintController {
    private var imgChecker = {
   		val img = new BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
      var x = 0
      while (x < 64) {
        var y = 0
        while (y < 64) {
          img.setRGB(x, y, if (((x / 32) ^ (y / 32)) == 0) 0xFF9F9F9F else 0xFF7F7F7F)
          y += 1
        }
        x += 1
      }
      img
    }

    private var pntChecker = new TexturePaint(imgChecker, new Rectangle(0, 0, 64, 64))

    private var paintFun: Graphics2D => Unit = paintChecker("Calculating...") _

    override def paintComponent(g: Graphics) {
      // println(s"paint $getWidth $getHeight")
      val g2 = g.asInstanceOf[Graphics2D]
      paintFun(g2)
    }

    private def paintChecker(message: String)(g2: Graphics2D) {
      g2.setPaint(pntChecker)
      g2.fillRect(0, 0, getWidth, getHeight)
      g2.setColor(Color.white)
      g2.drawString(message, 10, 20)
    }

    private def paintReady(g2: Graphics2D) {
      sono.paint(spanStart = 0, spanStop = sono.inputSpec.numFrames, g2, 0, 0, getWidth, getHeight, this)
    }

    def adjustGain(amp: Float, pos: Double) = amp

    def imageObserver = this

    import ExecutionContext.Implicits.global

    sono.onSuccess {
      case _ => println("SUCCESS")
    }

    sono.onFailure {
      case _ => println("FAILURE")
    }

    sono.onComplete {
      case Success(_) => /* println("SUCCESS"); */ execInGUI(ready())
      case Failure(e) => /* println("FAILURE"); */ execInGUI(failed(e))
    }

    private def ready() {
      paintFun    = paintReady _
      pntChecker  = null
      imgChecker.flush()
      imgChecker  = null
      repaint()
    }

    private def failed(exception: Throwable) {
      val message = s"${exception.getClass.getName} - ${exception.getMessage}"
      paintFun    = paintChecker(message)
      repaint()
    }
  }

  private val scroll = new JScrollBar(SwingConstants.HORIZONTAL)

  setLayout(new BorderLayout())
  private val meterPane   = Box.createVerticalBox()
  meters.foreach(meterPane add _)
  private val timePane    = Box.createHorizontalBox()
  timePane.add(Box.createHorizontalStrut(meterPane.getPreferredSize.width))
  private val scrollPane  = Box.createHorizontalBox()
  scrollPane.add(scroll)
  scrollPane.add(Box.createHorizontalStrut(16))
  add(meterPane,  BorderLayout.WEST)
  add(timePane,   BorderLayout.NORTH)
  add(SonoView,   BorderLayout.CENTER)
  add(scrollPane, BorderLayout.SOUTH)
}