/*
 *  ViewJ.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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
package audiofile

import de.sciss.{desktop, sonogram}
import java.awt.{Color, Graphics2D}
import de.sciss.synth.proc.{TimeRef, Timeline}

import scala.swing.event.MousePressed
import scala.util.Failure
import scala.util.Success
import scala.swing.{Action, Swing, Component}
import Swing._
import de.sciss.audiowidgets.TimelineModel
import de.sciss.audiowidgets.impl.TimelineCanvasImpl
import de.sciss.lucre.swing._

final class ViewJ(sono: sonogram.Overview, val timelineModel: TimelineModel)
  extends TimelineCanvasImpl {

  import TimelineCanvasImpl._

  // private val numChannels = sono.inputSpec.numChannels
  // private val minFreq     = sono.config.sonogram.minFreq
  // private val maxFreq     = sono.config.sonogram.maxFreq

  //  private val meters  = Vector.fill(numChannels) {
  //    val res   = new PeakMeterBar(javax.swing.SwingConstants.VERTICAL)
  //    res.ticks = 50
  //    res
  //  }

  object canvasComponent extends Component with sonogram.PaintController {
    private var paintFun: Graphics2D => Unit = paintChecker("Calculating...")
    private val srRatio = sono.inputSpec.sampleRate / TimeRef.SampleRate

    override def paintComponent(g: Graphics2D): Unit = {
      paintFun(g)
      paintPosAndSelection(g, height)
    }

    @inline def width   = peer.getWidth
    @inline def height  = peer.getHeight

    preferredSize = {
      val b = desktop.Util.maximumWindowBounds
      (b.width >> 1, b.height >> 1)
    }

    private def paintChecker(message: String)(g: Graphics2D): Unit = {
      g.setPaint(pntChecker)

      g.fillRect(0, 0, width, height)
      g.setColor(Color.white)
      g.drawString(message, 10, 20)
    }

    private def paintReady(g: Graphics2D): Unit = {
      val visSpan   = timelineModel.visible
      val fileStart = visSpan.start * srRatio
      val fileStop  = visSpan.stop  * srRatio
      sono.paint(spanStart = fileStart, spanStop = fileStop, g, 0, 0, width, height, this)
    }

    def adjustGain(amp: Float, pos: Double) = amp

    def imageObserver = peer

    private def ready(): Unit = {
      paintFun = paintReady
      repaint()
    }

    private def failed(exception: Throwable): Unit = {
      val message = s"${exception.getClass.getName} - ${exception.getMessage}"
      paintFun    = paintChecker(message)
      repaint()
    }

    // ---- constructor ----

    sono.onComplete {
      case Success(_) => /* println("SUCCESS"); */ defer(ready())
      case Failure(e) => /* println("FAILURE"); */ defer(failed(e))
    }

    listenTo(mouse.clicks)
    reactions += {
      case _: MousePressed => requestFocus()
    }
  }

  //  private val meterPane   = new BoxPanel(Orientation.Vertical) {
  //    meters.foreach(m => contents += Component.wrap(m))
  //  }
}