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

import javax.swing.undo.UndoableEdit

import de.sciss.desktop.impl.UndoManagerImpl
import de.sciss.desktop.{UndoManager, OptionPane, Window}
import de.sciss.lucre.stm
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.synth.Sys
import de.sciss.synth.{SynthGraph, proc}
import proc.Implicits._
import de.sciss.synth.proc.{ProcKeys, SynthGraphs, Proc, Obj}

object CodeFrameImpl {
  def proc[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])
                       (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    // if there is no source code attached,
    // create a new code object and add it to the attribute map.
    // let's just do that without undo manager
    val codeObj = proc.attr.get(ProcKeys.attrGraphSource) match {
      case Some(Code.Elem.Obj(c)) => c
      case _ =>
        val source  = "// graph function source code\n\n"
        val code    = Code.SynthGraph(source)
        val c       = Obj(Code.Elem(Code.Expr.newVar(Code.Expr.newConst[S](code))))
        proc.attr.put(ProcKeys.attrGraphSource, c)
        c
    }

    val codeEx0 = codeObj.elem.peer
    val procH   = tx.newHandle(proc.elem.peer)
    val code0   = codeEx0.value match {
      case cs: Code.SynthGraph => cs
      case other => sys.error(s"Proc source code does not produce SynthGraph: ${other.contextName}")
    }
    // val codeExH = tx.newHandle(_codeEx)
    val handler = new CodeView.Handler[S, Unit, SynthGraph] {
      def in = ()

      def save(out: SynthGraph)(implicit tx: S#Tx): UndoableEdit = {
        val proc = procH()
        import SynthGraphs.{serializer, varSerializer}
        EditVar.Expr[S, SynthGraph]("Change SynthGraph", proc.graph, SynthGraphs.newConst[S](out))
      }

      def dispose()(implicit tx: S#Tx) = ()
    }

    make(codeObj, code0, proc.attr.name, Some(handler))
  }

  def apply[S <: Sys[S]](obj: Obj.T[S, Code.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    val _codeEx = obj.elem.peer
    val _code   = _codeEx.value
    make[S, _code.In, _code.Out](obj, _code, obj.attr.name, None)
  }

  private def make[S <: Sys[S], In0, Out0](obj: Obj.T[S, Code.Elem], code0: Code { type In = In0; type Out = Out0 },
                                _name: String, handler: Option[CodeView.Handler[S, In0, Out0]])
                               (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    implicit val undoMgr: UndoManager = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    // val _name   = /* title getOrElse */ obj.attr.name
    val view    = CodeView(obj, code0)(handler)
    val res     = new FrameImpl(view, name0 = _name, contextName = code0.contextName)
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