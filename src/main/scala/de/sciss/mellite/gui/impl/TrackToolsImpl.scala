package de.sciss
package mellite
package gui
package impl

import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.Sys
import de.sciss.lucre.event.Change

final class TrackToolsImpl[S <: Sys[S]](timelineModel: TimelineModel)
  extends TrackTools[S] with ModelImpl[TrackTools.Update[S]] {

  import TrackTools._

  private var _currentTool: TrackTool[S] = new TrackCursorTool[S](timelineModel)
  def currentTool = _currentTool
  def currentTool_=(value: TrackTool[S]) {
    if (_currentTool != value) {
      val oldTool   = _currentTool
      _currentTool  = value
      dispatch(ToolChanged(Change(oldTool, value)))
    }
  }

  private var _visualBoost: Float = 1f
  def visualBoost = _visualBoost
  def visualBoost_=(value: Float) {
    if (_visualBoost != value) {
      val oldBoost  = _visualBoost
      _visualBoost  = value
      dispatch(VisualBoostChanged(Change(oldBoost, value)))
    }
  }

  private var _fadeViewMode: FadeViewMode = FadeViewMode.Curve
  def fadeViewMode = _fadeViewMode
  def fadeViewMode_=(value: FadeViewMode) {
    if (_fadeViewMode != value) {
      val oldMode   = _fadeViewMode
      _fadeViewMode = value
      dispatch(FadeViewModeChanged(Change(oldMode, value)))
    }
  }

  private var _regionViewMode: RegionViewMode = RegionViewMode.TitledBox
  def regionViewMode = _regionViewMode
  def regionViewMode_=(value: RegionViewMode) {
    if (_regionViewMode != value) {
      val oldMode     = _regionViewMode
      _regionViewMode = value
      dispatch(RegionViewModeChanged(Change(oldMode, value)))
    }
  }
}