/*
 *  ToolsImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl
package tracktool

import de.sciss.model.impl.ModelImpl
import de.sciss.synth.proc.Sys
import de.sciss.lucre.event.Change
import javax.swing.ImageIcon

object ToolsImpl {
  def getIcon(name: String) = new ImageIcon(Mellite.getClass.getResource(s"icon-$name.png"))
}
final class ToolsImpl[S <: Sys[S]](canvas: TimelineProcCanvas[S])
  extends TrackTools[S] with ModelImpl[TrackTools.Update[S]] {

  import TrackTools._

  private var _currentTool: TrackTool[S, _] = TrackTool.cursor(canvas)
  def currentTool = _currentTool
  def currentTool_=(value: TrackTool[S, _]): Unit =
    if (_currentTool != value) {
      val oldTool   = _currentTool
      _currentTool  = value
      oldTool.uninstall(canvas.canvasComponent)
      value    .install(canvas.canvasComponent)
      dispatch(ToolChanged(Change(oldTool, value)))
    }

  private var _visualBoost: Float = 1f
  def visualBoost = _visualBoost
  def visualBoost_=(value: Float): Unit =
    if (_visualBoost != value) {
      val oldBoost  = _visualBoost
      _visualBoost  = value
      dispatch(VisualBoostChanged(Change(oldBoost, value)))
    }

  private var _fadeViewMode: FadeViewMode = FadeViewMode.Curve
  def fadeViewMode = _fadeViewMode
  def fadeViewMode_=(value: FadeViewMode): Unit =
    if (_fadeViewMode != value) {
      val oldMode   = _fadeViewMode
      _fadeViewMode = value
      dispatch(FadeViewModeChanged(Change(oldMode, value)))
    }

  private var _regionViewMode: RegionViewMode = RegionViewMode.TitledBox
  def regionViewMode = _regionViewMode
  def regionViewMode_=(value: RegionViewMode): Unit =
    if (_regionViewMode != value) {
      val oldMode     = _regionViewMode
      _regionViewMode = value
      dispatch(RegionViewModeChanged(Change(oldMode, value)))
    }
}