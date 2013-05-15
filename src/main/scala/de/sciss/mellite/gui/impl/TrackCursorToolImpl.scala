package de.sciss
package mellite
package gui
package impl

import synth.proc.Sys
import java.awt.Cursor
import de.sciss.model.impl.ModelImpl
import scala.swing.Component

final class TrackCursorToolImpl[S <: Sys[S]](canvas: TimelineProcCanvas[S])
  extends TrackTool[S, Unit] with ModelImpl[TrackTool.Update[Unit]] {

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR)
  val name          = "Cursor"
  val icon          = TrackToolsImpl.getIcon("text")

  def install(component: Component) {
    component.cursor = defaultCursor
  }

  def uninstall(component: Component) {
    component.cursor = null
  }

  def commit(drag: Unit)(implicit tx: S#Tx) {}
}
