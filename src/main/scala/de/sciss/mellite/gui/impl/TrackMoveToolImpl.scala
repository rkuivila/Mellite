package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import java.awt.Cursor

final class TrackMoveToolImpl[S <: Sys[S]](protected val timelineModel: TimelineModel,
                                           protected val selectionModel: ProcSelectionModel[S])
  extends BasicTrackRegionTool[S, TrackTool.Move] {

  import TrackTool._

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.HAND_CURSOR)
  val name          = "Move"

  protected def dragToParam(d: Drag): Move = {
    Move(deltaTime = d.currentPos - d.firstPos, deltaTrack = d.currentTrack - d.firstTrack,
      copy = d.currentEvent.isAltDown)
  }

  protected def dialog(): Option[Move] = {
    //    val box             = Box.createHorizontalBox
    //    val timeTrans       = new DefaultUnitTranslator()
    //    val ggTime          = new BasicParamField(timeTrans)
    //    val spcTimeHHMMSSD  = new ParamSpace(Double.NegativeInfinity, Double.PositiveInfinity, 0.0, 1, 3, 0.0,
    //      ParamSpace.TIME | ParamSpace.SECS | ParamSpace.HHMMSS | ParamSpace.OFF)
    //    ggTime.addSpace(spcTimeHHMMSSD)
    //    ggTime.addSpace(ParamSpace.spcTimeSmpsD)
    //    ggTime.addSpace(ParamSpace.spcTimeMillisD)
    //    GUIUtil.setInitialDialogFocus(ggTime)
    //    box.add(new JLabel("Move by:"))
    //    box.add(Box.createHorizontalStrut(8))
    //    box.add(ggTime)
    //
    //    val tl = timelineModel.timeline
    //    timeTrans.setLengthAndRate(tl.span.length, tl.rate)
    //    if (showDialog(box)) {
    //      val delta = timeTrans.translate(ggTime.value, ParamSpace.spcTimeSmpsD).value.toLong
    //      Some(Move(delta, 0, copy = false))
    //    } else
    None
  }
}
