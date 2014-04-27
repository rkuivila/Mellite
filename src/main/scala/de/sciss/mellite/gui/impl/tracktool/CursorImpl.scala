/*
 *  CursorImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl
package tracktool

import java.awt.Cursor
import de.sciss.synth.proc.{Obj, ProcElem}
import java.awt.event.MouseEvent
import de.sciss.mellite.gui.impl.timeline.ProcView
import de.sciss.lucre.expr.Expr
import de.sciss.span.{Span, SpanLike}
import de.sciss.desktop.OptionPane
import scala.swing.{Label, FlowPanel, TextField}
import de.sciss.lucre.synth.Sys

final class CursorImpl[S <: Sys[S]](val canvas: TimelineProcCanvas[S]) extends RegionImpl[S, TrackTool.Cursor] {

  def defaultCursor = Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR)
  val name          = "Cursor"
  val icon          = ToolsImpl.getIcon("text")

  protected def handleSelect(e: MouseEvent, hitTrack: Int, pos: Long, region: ProcView[S]): Unit =
    if (e.getClickCount == 2) {
      val ggText  = new TextField(region.name, 24)
      val panel   = new FlowPanel(new Label("Name:"), ggText)

      val pane    = OptionPane(panel, OptionPane.Options.OkCancel, OptionPane.Message.Question, focus = Some(ggText))
      pane.title  = "Rename Region"
      val res     = pane.show(None) // XXX TODO: search for window source
      if (res == OptionPane.Result.Ok && ggText.text != region.name) {
        val text    = ggText.text
        val nameOpt = if (text == "" || text == ProcView.Unnamed) None else Some(text)
        dispatch(TrackTool.Adjust(TrackTool.Cursor(nameOpt)))
      }

    } else {
      canvas.timelineModel.modifiableOption.foreach { mod =>
        val it    = canvas.selectionModel.iterator
        val empty = Span.Void: Span.SpanOrVoid
        val all   = (empty /: it) { (res, pv) =>
          pv.span match {
            case sp @ Span(_, _) => res.nonEmptyOption.fold(sp)(_ union sp)
          }
        }
        mod.selection = all
      }
    }

  protected def commitProc(drag: TrackTool.Cursor)(span: Expr[S, SpanLike], proc: Obj.T[S, ProcElem])
                          (implicit tx: S#Tx): Unit =
    ProcActions.rename(proc, drag.name)
}