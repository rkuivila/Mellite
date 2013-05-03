package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import model.impl.ModelImpl
import mellite.gui.ProcSelectionModel
import java.awt.event.MouseEvent

trait TrackRegionToolImpl[S <: Sys[S], A] extends TrackTool[S, A] with ModelImpl[TrackTool.Update[A]] {
  tool =>

  // protected def trackList: TrackList
  protected def timelineModel: TimelineModel
  protected def selectionModel: ProcSelectionModel[S]

  def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineProcView[S]]) {
    if (e.isShiftDown) {
      regionOpt.foreach { region =>
        if (selectionModel.contains(region)) {
          selectionModel -= region
        } else {
          selectionModel += region
        }
      }
    } else {
      if (regionOpt.map(region => !selectionModel.contains(region)) getOrElse true) {
        // either hitten a region which wasn't selected, or hitting an empty area
        // --> deselect all
        selectionModel.clear()
        regionOpt.foreach(selectionModel += _)
      }
    }

    // now go on if region is selected
    regionOpt.foreach(region => if (selectionModel.contains(region)) {
      handleSelect(e, hitTrack, pos, region)
    })
  }

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: TimelineProcView[S]): Unit

  protected def screenToVirtual(e: MouseEvent): Long = {
    val tlSpan = timelineModel.visible // bounds
    val p_off = -tlSpan.start
    val p_scale = e.getComponent.getWidth.toDouble / tlSpan.length
    (e.getX.toLong / p_scale - p_off + 0.5).toLong
  }
}