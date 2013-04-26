package de.sciss.mellite
package gui
package impl

import de.sciss.sonogram
import java.awt.image.BufferedImage
import java.awt.{Color, Graphics2D, Rectangle, TexturePaint}
import de.sciss.audiowidgets.j.PeakMeterBar
import de.sciss.audiowidgets.{AxisFormat, Axis}
import scala.concurrent.ExecutionContext
import scala.util.Failure
import scala.util.Success
import scala.swing.{Reactions, BoxPanel, Orientation, Component, Swing, BorderPanel}
import Swing._
import scala.swing.event.{Key, MouseDragged, MousePressed, ValueChanged, UIElementResized}
import de.sciss.span.Span

object AudioFileViewJ {
  private sealed trait AxisMouseAction
  private case object AxisPosition extends AxisMouseAction
  private final case class AxisSelection(fix: Long) extends AxisMouseAction

  private val colrSelection     = new Color(0x00, 0x00, 0xFF, 0x4F)
  private val colrPosition      = Color.white // new Color(0x00, 0x00, 0xFF, 0x7F)
  // private val colrSelection2    = new Color(0x00, 0x00, 0x00, 0x40)
  // private val colrPlayHead      = new Color(0x00, 0xD0, 0x00, 0xC0)
}
final class AudioFileViewJ(sono: sonogram.Overview, protected val timelineModel: TimelineModel)
  extends BorderPanel with TimelineNavigation with DynamicComponentImpl {

  import AudioFileViewJ._

  private val numChannels = sono.inputSpec.numChannels
  // private val minFreq     = sono.config.sonogram.minFreq
  // private val maxFreq     = sono.config.sonogram.maxFreq

  private val r = new Rectangle

  private def paintPosAndSelection(g: Graphics2D, h: Int) {
    val pos = frameToScreen(timelineModel.position).toInt
    g.getClipBounds(r)
    val rr  = r.x + r.width
    timelineModel.selection match {
      case Span(start, stop) =>
        val selx1 = frameToScreen(start).toInt
        val selx2 = frameToScreen(stop ).toInt
        if (selx1 < rr && selx2 > r.x) {
          g.setColor(colrSelection)
          g.fillRect(selx1, 0, selx2 - selx1, h)
          if (r.x <= selx1) g.drawLine(selx1, 0, selx1, h)
          if (selx2 > selx1 && rr >= selx2) g.drawLine(selx2 - 1, 0, selx2 - 1, h)
        }
      case _ =>
    }
    if (r.x <= pos && rr > pos) {
      g.setXORMode(colrPosition)
      g.drawLine(pos, 0, pos, h)
      g.setPaintMode()
    }
  }

  private val timeAxis = {
    val res       = new Axis {
      override protected def paintComponent(g: Graphics2D) {
        super.paintComponent(g)
        paintPosAndSelection(g, peer.getHeight)
      }
    }
    val maxSecs   = timelineModel.span.stop  / timelineModel.sampleRate
    res.format    = AxisFormat.Time(hours = maxSecs >= 3600.0, millis = true)
    res
  }

  private def updateAxis() {
    val visi  = timelineModel.visible
    val sr    = timelineModel.sampleRate
    timeAxis.minimum   = visi.start / sr
    timeAxis.maximum   = visi.stop  / sr
  }

  // XXX TODO: Axis lost its logarithmic scale a while back. Need to reimplement
  //  private val freqAxes  = Vector.fill(numChannels) {
  //    val res       = new Axis(SwingConstants.VERTICAL)
  //    res.minimum   = minFreq
  //    res.maximum   = maxFreq
  //  }

  private val meters  = Vector.fill(numChannels) {
    val res   = new PeakMeterBar(javax.swing.SwingConstants.VERTICAL)
    res.ticks = 50
    res
  }

  private object SonoView extends Component with sonogram.PaintController {
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

    override def paintComponent(g: Graphics2D) {
      paintFun(g)
      paintPosAndSelection(g, height)
    }

    @inline def width   = peer.getWidth
    @inline def height  = peer.getHeight

    private def paintChecker(message: String)(g2: Graphics2D) {
      g2.setPaint(pntChecker)

      g2.fillRect(0, 0, width, height)
      g2.setColor(Color.white)
      g2.drawString(message, 10, 20)
    }

    private def paintReady(g2: Graphics2D) {
      val visi = timelineModel.visible
      sono.paint(spanStart = visi.start, spanStop = visi.stop, g2, 0, 0, width, height, this)
    }

    def adjustGain(amp: Float, pos: Double) = amp

    def imageObserver = peer

    import ExecutionContext.Implicits.global

    //    sono.onSuccess {
    //      case _ => println("SUCCESS")
    //    }
    //
    //    sono.onFailure {
    //      case _ => println("FAILURE")
    //    }

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

  private val scroll      = new ScrollBarImpl {
    orientation   = Orientation.Horizontal
    unitIncrement = 4
  }

  private def frameToScreen(frame: Long): Double = {
    val visi = timelineModel.visible
    (frame - visi.start).toDouble / visi.length * SonoView.width
  }

  private def screenToFrame(screen: Int): Double = {
    val visi = timelineModel.visible
    screen.toDouble / SonoView.width * visi.length + visi.start
  }

  private def clipVisible(frame: Double): Long = {
    val visi = timelineModel.visible
    visi.clip(frame.toLong)
  }

  private def updateScroll() {
    val trackWidth      = math.max(1, scroll.peer.getWidth - 32)  // TODO XXX stupid hard coded value. but how to read it?
    val visi            = timelineModel.visible
    val total           = timelineModel.span
    val framesPerPixel  = math.max(1, ((total.length + (trackWidth >> 1)) / trackWidth).toInt)
    val max             = math.min(0x3FFFFFFFL, (total.length / framesPerPixel)).toInt
    val pos             = math.min(max - 1, (visi.start - total.start) / framesPerPixel).toInt
    val visiAmt         = math.min(max - pos, visi.length / framesPerPixel).toInt
    val blockInc        = math.max(1, visiAmt * 4 / 5)
    // val unitInc         = 4

    // __DO NOT USE deafTo and listenTo__ there must be a bug in scala-swing,
    // because that quickly overloads the AWT event multicaster with stack overflows.
    //    deafTo(scroll)
    val l = isListening
    if (l) reactions -= scrollListener
    scroll.maximum        = max
    scroll.visibleAmount  = visiAmt
    scroll.value          = pos
    scroll.blockIncrement = blockInc
    //    listenTo(scroll)
    if (l) reactions += scrollListener
  }

  private def updateFromScroll() {
    val visi              = timelineModel.visible
    val total             = timelineModel.span
    val pos               = math.min(total.stop - visi.length,
      ((scroll.value.toDouble / scroll.maximum) * total.length + 0.5).toLong)
    val l = isListening
    if (l) timelineModel.removeListener(timelineListener)
    val newVisi = Span(pos, pos + visi.length)
    // println(s"updateFromScroll : $newVisi")
    timelineModel.visible = newVisi
    updateAxis()
    SonoView.repaint()
    if (l) timelineModel.addListener(timelineListener)
  }

  private val meterPane   = new BoxPanel(Orientation.Vertical) {
    meters.foreach(m => contents += Component.wrap(m))
  }
  private val timePane    = new BoxPanel(Orientation.Horizontal) {
    contents += HStrut(meterPane.preferredSize.width)
    contents += timeAxis
  }
  private val scrollPane = new BoxPanel(Orientation.Horizontal) {
    contents += scroll
    contents += HStrut(16)
  }
  add(meterPane,  BorderPanel.Position.West  )
  add(timePane,   BorderPanel.Position.North )
  add(SonoView,   BorderPanel.Position.Center)
  add(scrollPane, BorderPanel.Position.South )

  protected def component = this

  private val timelineListener: TimelineModel.Listener = {
    case TimelineModel.Visible(_, span) =>
      updateAxis()
      updateScroll()
      SonoView.repaint()  // XXX TODO: optimize dirty region / copy double buffer

    case TimelineModel.Position(_, frame) =>
      // XXX TODO: optimize dirty region
      timeAxis.repaint()
      SonoView.repaint()

    case TimelineModel.Selection(_, span) =>
      // XXX TODO: optimize dirty region
      timeAxis.repaint()
      SonoView.repaint()
  }

  private val scrollListener: Reactions.Reaction = {
    case UIElementResized(`scroll`) =>
      updateScroll()

    case ValueChanged(`scroll`) =>
      // println(s"ScrollBar Value ${scroll.value}")
      updateFromScroll()
  }

  protected def componentShown() {
    timelineModel.addListener(timelineListener)
    updateAxis()
    updateScroll()  // this adds scrollListener in the end
  }

  protected def componentHidden() {
    timelineModel.removeListener(timelineListener)
    reactions -= scrollListener
  }

  private var axisMouseAction: AxisMouseAction = AxisPosition

  private def processAxisMouse(frame: Long) {
    axisMouseAction match {
      case AxisPosition =>
        timelineModel.position = frame
      case AxisSelection(fix) =>
        val span = Span(math.min(frame, fix), math.max(frame, fix))
        timelineModel.selection = if (span.isEmpty) Span.Void else span
    }
  }

  listenTo(scroll)
  listenTo(timeAxis.mouse.clicks)
  listenTo(timeAxis.mouse.moves)
  reactions += {
    case MousePressed(`timeAxis`, point, mod, _, _) =>
      // no mods: move position; shift: extend selection; alt: clear selection
      val frame = clipVisible(screenToFrame(point.x))
      if ((mod & Key.Modifier.Alt) != 0) {
        timelineModel.selection = Span.Void
      }
      if ((mod & Key.Modifier.Shift) != 0) {
        val otra = timelineModel.selection match {
          case Span.Void          => timelineModel.position
          case Span(start, stop)  => if (math.abs(frame - start) > math.abs(frame - stop)) start else stop
        }
        axisMouseAction = AxisSelection(otra)
      } else {
        axisMouseAction = AxisPosition
      }
      processAxisMouse(frame)

    case MouseDragged(`timeAxis`, point, _) =>
      val frame = clipVisible(screenToFrame(point.x))
      processAxisMouse(frame)
  }
}