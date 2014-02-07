/*
 *  ProcCanvasImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl
package timeline

import de.sciss.audiowidgets.impl.TimelineCanvasImpl
import de.sciss.lucre.synth.Sys

trait ProcCanvasImpl[S <: Sys[S]] extends TimelineCanvasImpl with TimelineProcCanvas[S] {
  final val trackTools = TrackTools[S](this)

  import TrackTools._

  //  private var _toolState = Option.empty[Any]
  //  final protected def toolState = _toolState

  protected var toolState: Option[Any]

  private val toolListener: TrackTool.Listener = {
    // case TrackTool.DragBegin =>
    case TrackTool.DragCancel =>
      log(s"Drag cancel $toolState")
      if (toolState.isDefined) {
        toolState = None
        repaint()
      }
    case TrackTool.DragEnd =>
      log(s"Drag end $toolState")
      toolState.foreach { state =>
        toolState = None
        commitToolChanges(state)
        repaint()
      }

    case TrackTool.DragAdjust(value) =>
      // log(s"Drag adjust $value")
      val some = Some(value)
      if (toolState != some) {
        toolState = some
        repaint()
      }

    case TrackTool.Adjust(state) =>
      log(s"Tool commit $state")
      toolState = None
      commitToolChanges(state)
      repaint()
  }

  trackTools.addListener {
    case ToolChanged(change) =>
      change.before.removeListener(toolListener)
      change.now   .addListener   (toolListener)
    case VisualBoostChanged   (change) => repaint()
    case FadeViewModeChanged  (change) => repaint()
    case RegionViewModeChanged(change) => repaint()
  }
  trackTools.currentTool.addListener(toolListener)

  private val selectionListener: ProcSelectionModel.Listener[S] = {
    case ProcSelectionModel.Update(added, removed) =>
      canvasComponent.repaint() // XXX TODO: dirty rectangle optimization
  }

  override protected def componentShown(): Unit = {
    super.componentShown()
    selectionModel.addListener(selectionListener)
  }

  override protected def componentHidden(): Unit = {
    super.componentHidden()
    selectionModel.removeListener(selectionListener)
  }

  protected def commitToolChanges(value: Any): Unit
}