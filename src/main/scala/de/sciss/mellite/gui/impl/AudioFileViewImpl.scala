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
import java.awt.{Graphics2D, Graphics}

object AudioFileViewImpl {
  private lazy val manager = sonogram.OverviewManager()

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

      override def paintComponents(g: Graphics) {
        val g2 = g.asInstanceOf[Graphics2D]
        sono.paint(spanStart = 0, spanStop = sono.inputSpec.numFrames, g2, 0, 0, getWidth, getHeight, this)
      }

      def adjustGain(amp: Float, pos: Double) = amp

      def imageObserver = this
    }

    def guiInit(f: File) {
      val sono  = manager.acquire(sonogram.OverviewManager.Job(f))
      val view  = new View(sono)
      component = Component.wrap(view)
    }

    def element(implicit tx: S#Tx): AudioGrapheme[S] = holder()
  }
}