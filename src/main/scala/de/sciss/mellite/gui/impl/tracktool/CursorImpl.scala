package de.sciss.mellite
package gui
package impl
package tracktool

import java.awt.Cursor
import de.sciss.model.impl.ModelImpl
import scala.swing.Component
import de.sciss.synth.proc.Sys

final class CursorImpl[S <: Sys[S]](canvas: TimelineProcCanvas[S])
  extends TrackTool[S, Unit] with ModelImpl[TrackTool.Update[Unit]] {

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR)
  val name          = "Cursor"
  val icon          = ToolsImpl.getIcon("text")

  def install(component: Component): Unit =
    component.cursor = defaultCursor

  def uninstall(component: Component): Unit =
    component.cursor = null

  def commit(drag: Unit)(implicit tx: S#Tx) = ()
}
