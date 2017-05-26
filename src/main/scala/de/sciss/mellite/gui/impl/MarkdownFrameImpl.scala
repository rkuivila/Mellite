/*
 *  MarkdownFrameImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
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

import de.sciss.desktop.OptionPane
import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.lucre.stm
import de.sciss.lucre.swing.View
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{Markdown, Workspace}

import scala.collection.immutable.{Seq => ISeq}

object MarkdownFrameImpl {
  def editor[S <: Sys[S]](obj: Markdown[S], bottom: ISeq[View[S]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): MarkdownEditorFrame[S] = {
    implicit val undo = new UndoManagerImpl
    val view  = MarkdownEditorView(obj, bottom = bottom)
    val res   = new EditorFrameImpl(view).init()
    trackTitle(res, view.renderer)
    res
  }

  def render[S <: Sys[S]](obj: Markdown[S])
                         (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): MarkdownRenderFrame[S] = {
    val view  = MarkdownRenderView(obj)
    val res   = new RenderFrameImpl(view).init()
    trackTitle(res, view)
    res
  }

  private def setTitle[S <: Sys[S]](win: WindowImpl[S], md: Markdown[S])(implicit tx: S#Tx): Unit =
    win.setTitleExpr(Some(AttrCellView.name(md)))

  private def trackTitle[S <: Sys[S]](win: WindowImpl[S], renderer: MarkdownRenderView[S])(implicit tx: S#Tx): Unit = {
    setTitle(win, renderer.markdown)
    renderer.react { implicit tx => {
      case MarkdownRenderView.FollowedLink(_, now) => setTitle(win, now)
    }}
  }

  // ---- frame impl ----

  private final class RenderFrameImpl[S <: Sys[S]](val view: MarkdownRenderView[S])
    extends WindowImpl[S] with MarkdownRenderFrame[S] {

  }

  private final class EditorFrameImpl[S <: Sys[S]](val view: MarkdownEditorView[S])
    extends WindowImpl[S] with MarkdownEditorFrame[S] {

    override protected def checkClose(): Boolean =
      !view.dirty || {
        val message = "The text has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close - $title"
        opt.show(Some(window)) match {
          case OptionPane.Result.No => true
          case OptionPane.Result.Yes =>
            /* val fut = */ view.save()
            true

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            false
        }
      }
  }
}