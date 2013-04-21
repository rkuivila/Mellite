package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.Sys
import de.sciss.lucre.stm
import Element.AudioGrapheme
import swing.Component
import de.sciss.sonogram
import javax.swing.JComponent
import java.io.File
import java.awt.{Color, Rectangle, TexturePaint, Paint, Graphics2D, Graphics}
import scala.util.{Failure, Success}
import java.awt.image.BufferedImage

object AudioFileViewImpl {
  private lazy val manager = {
    val cfg       = sonogram.OverviewManager.Config()
    val folder    = new File(new File(sys.props("user.home"), "mellite"), "cache")
    folder.mkdirs()
    val sizeLimit = 2L << 10 << 10 << 10  // 2 GB
    cfg.caching = Some(sonogram.OverviewManager.Caching(folder, sizeLimit))
    sonogram.OverviewManager()
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

    private final class View(sono: sonogram.Overview)
      extends JComponent with sonogram.PaintController {

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

      def ready() {
        paintFun    = paintReady _
        pntChecker  = null
        imgChecker.flush()
        imgChecker  = null
        repaint()
      }

      def failed(exception: Throwable) {
        val message = s"${exception.getClass.getName} - ${exception.getMessage}"
        paintFun    = paintChecker(message)
        repaint()
      }

      private def paintReady(g2: Graphics2D) {
        sono.paint(spanStart = 0, spanStop = sono.inputSpec.numFrames, g2, 0, 0, getWidth, getHeight, this)
      }

      def adjustGain(amp: Float, pos: Double) = amp

      def imageObserver = this
    }

    def guiInit(f: File) {
      // println("AudioFileView guiInit")
      val sono  = manager.acquire(sonogram.OverviewManager.Job(f))
      val view  = new View(sono)
      implicit val exec = manager.config.executionContext
      sono.onComplete {
        case Success(_) => execInGUI(view.ready())
        case Failure(e) => execInGUI(view.failed(e))
      }
      component = Component.wrap(view)
    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }
}