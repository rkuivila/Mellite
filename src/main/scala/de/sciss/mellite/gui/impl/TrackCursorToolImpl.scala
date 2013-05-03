package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import java.awt.Cursor
import java.awt.event.MouseEvent
import de.sciss.model.impl.ModelImpl

final class TrackCursorToolImpl[S <: Sys[S]](timelineModel: TimelineModel)
  extends TrackTool[S, Unit] with ModelImpl[TrackTool.Update[Unit]] {

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR)
  val name          = "Cursor"

  def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, regionOpt: Option[TimelineProcView[S]]) {}
}
