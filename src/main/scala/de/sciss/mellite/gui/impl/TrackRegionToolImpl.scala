package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import model.impl.ModelImpl
import mellite.gui.ProcSelectionModel
import java.awt.event.{MouseAdapter, MouseEvent}
import javax.swing.ImageIcon
import de.sciss.span.Span
import scala.swing.Component

trait TrackRegionToolImpl[S <: Sys[S], A] extends TrackTool[S, A] with ModelImpl[TrackTool.Update[A]] {
  tool =>

  // protected def trackList: TrackList
  protected def canvas: TimelineProcCanvas[S]

  private val mia = new MouseAdapter {
    override def mousePressed(e: MouseEvent) {
      val pos       = canvas.screenToFrame(e.getX).toLong
      val span      = Span(pos, pos + 1)
      val regions   = canvas.intersect(span)
      val hitTrack  = e.getY / 32
      val regionOpt = regions.find(pv => pv.track == hitTrack || (pv.track + 1) == hitTrack)  // procs span "two tracks". ouchilah...
      handleSelect(e, hitTrack, pos, regionOpt)
      // if ((e.getClickCount == 2) && !regions.isEmpty) showObserverPage()
    }
  }

  final def uninstall(component: Component) {
    component.peer.removeMouseListener(mia)
    component.peer.removeMouseMotionListener(mia)
    component.cursor = null
  }

  final def install(component: Component) {
    component.peer.addMouseListener(mia)
    component.peer.addMouseMotionListener(mia)
    component.cursor = defaultCursor
  }

  private def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineProcView[S]]) {
    val selm = canvas.selectionModel
    if (e.isShiftDown) {
      regionOpt.foreach { region =>
        if (selm.contains(region)) {
          selm -= region
        } else {
          selm += region
        }
      }
    } else {
      if (regionOpt.map(region => !selm.contains(region)) getOrElse true) {
        // either hitten a region which wasn't selected, or hitting an empty area
        // --> deselect all
        selm.clear()
        regionOpt.foreach(selm += _)
      }
    }

    // now go on if region is selected
    regionOpt.foreach(region => if (selm.contains(region)) {
      handleSelect(e, hitTrack, pos, region)
    })
  }

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView[S]): Unit

  //  protected def screenToVirtual(e: MouseEvent): Long = {
  //    val tlSpan = timelineModel.visible // bounds
  //    val p_off = -tlSpan.start
  //    val p_scale = e.getComponent.getWidth.toDouble / tlSpan.length
  //    (e.getX.toLong / p_scale - p_off + 0.5).toLong
  //  }
}