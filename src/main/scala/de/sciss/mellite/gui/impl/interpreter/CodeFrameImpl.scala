/*
 *  CodeFrameImpl.scala
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
package interpreter

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{UndoManager, OptionPane, Window}
import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc
import proc.Implicits._
import de.sciss.synth.proc.Obj

object CodeFrameImpl {
  def apply[S <: Sys[S]](obj: Obj.T[S, Code.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    implicit val undoMgr: UndoManager = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    val _name   = obj.attr.name
    val _codeEx = obj.elem.peer
    val _code   = _codeEx.value
    val view    = CodeView(obj, _code)(None)
    val res     = new FrameImpl(view, name0 = _name, contextName = _code.contextName)
    res.init()
    res
  }

  private final class FrameImpl[S <: Sys[S]](val view: CodeView[S], name0: String, contextName: String)
    extends WindowImpl[S](s"$name0 : $contextName Code") with CodeFrame[S] {

    private var name = name0

    override protected def checkClose(): Boolean = {
      if (view.isCompiling) {
        // ggStatus.text = "busy!"
        false
      }

      !view.dirty || {
        val message = "The code has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close Code Editor - $name"
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

    override def style = Window.Auxiliary
  }
}