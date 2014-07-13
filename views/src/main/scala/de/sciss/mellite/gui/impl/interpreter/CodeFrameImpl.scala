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
import de.sciss.lucre.stm.IDPeek
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.event.Sys
import de.sciss.mellite.impl.ActionImpl
import de.sciss.synth.{SynthGraph, proc}
import proc.Implicits._
import de.sciss.synth.proc.{ProcKeys, SynthGraphs, Proc, Obj}

object CodeFrameImpl {
  // ---- adapter for editing a Proc's source ----

  def proc[S <: Sys[S]](obj: Obj.T[S, Proc.Elem])
                       (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    val codeObj = mkSource(obj = obj, codeID = Code.SynthGraph.id, key = ProcKeys.attrGraphSource,
      init = "// graph function source code\n\n")
    
    val codeEx0 = codeObj.elem.peer
    val objH    = tx.newHandle(obj.elem.peer)
    val code0   = codeEx0.value match {
      case cs: Code.SynthGraph => cs
      case other => sys.error(s"Proc source code does not produce SynthGraph: ${other.contextName}")
    }

    val handler = new CodeView.Handler[S, Unit, SynthGraph] {
      def in() = ()

      def save(in: Unit, out: SynthGraph)(implicit tx: S#Tx): UndoableEdit = {
        val obj = objH()
        import SynthGraphs.{serializer, varSerializer}
        EditVar.Expr[S, SynthGraph]("Change SynthGraph", obj.graph, SynthGraphs.newConst[S](out))
      }

      def dispose()(implicit tx: S#Tx) = ()
      def execute()(implicit tx: S#Tx) = ()
    }

    make(codeObj, code0, obj.attr.name, Some(handler), hasExecute = false)
  }

  // ---- adapter for editing a Action's source ----

  def action[S <: Sys[S]](obj: Action.Obj[S])
                         (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    val codeObj = mkSource(obj = obj, codeID = Code.Action.id, key = Action.attrSource,
      init = "// action source code\n\n")

    val codeEx0 = codeObj.elem.peer
    val code0   = codeEx0.value match {
      case cs: Code.Action => cs
      case other => sys.error(s"Action source code does not produce plain function: ${other.contextName}")
    }

    val handlerOpt = obj.elem.peer match {
      case Action.Var(vr) =>
        val objH = tx.newHandle(vr)
        val handler = new CodeView.Handler[S, String, Array[Byte]] {
          def in(): String = cursor.step { implicit tx =>
            val id = tx.newID()
            val cnt = IDPeek(id)
            s"Action$cnt"
          }

          def save(in: String, out: Array[Byte])(implicit tx: S#Tx): UndoableEdit = {
            val obj = objH()
            val value = ActionImpl.newConst[S](name = in, jar = out)
            EditVar[S, Action[S], Action.Var[S]](name = "Change Action Body", expr = obj, value = value)
          }

          def execute()(implicit tx: S#Tx): Unit = {
            val obj = objH()
            obj.execute()
            // ActionImpl.execute[S](name = in, jar = out)
          }

          def dispose()(implicit tx: S#Tx) = ()
        }
        Some(handler)

      case _ => None
    }

    make(codeObj, code0, obj.attr.name, handlerOpt, hasExecute = true)
  }

  // ---- general constructor ----

  def apply[S <: Sys[S]](obj: Obj.T[S, Code.Elem], hasExecute: Boolean)
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    val _codeEx = obj.elem.peer
    val _code   = _codeEx.value
    make[S, _code.In, _code.Out](obj, _code, obj.attr.name, None, hasExecute = hasExecute)
  }
  
  private def make[S <: Sys[S], In0, Out0](obj: Code.Obj[S], code0: Code { type In = In0; type Out = Out0 },
                                _name: String, handler: Option[CodeView.Handler[S, In0, Out0]], hasExecute: Boolean)
                               (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] = {
    implicit val undoMgr: UndoManager = new UndoManagerImpl {
      protected var dirty: Boolean = false
    }
    // val _name   = /* title getOrElse */ obj.attr.name
    val view    = CodeView(obj, code0, hasExecute = hasExecute)(handler)
    val res     = new FrameImpl(view, name0 = _name, contextName = code0.contextName)
    res.init()
    res
  }

  // ---- util ----

  private def mkSource[S <: Sys[S]](obj: Obj[S], codeID: Int, key: String, init: String)(implicit tx: S#Tx): Code.Obj[S] = {
    // if there is no source code attached,
    // create a new code object and add it to the attribute map.
    // let's just do that without undo manager
    val codeObj = obj.attr.get(key) match {
      case Some(Code.Obj(c)) => c
      case _ =>
        val source  = init
        val code    = Code(codeID, source)
        val c       = Obj(Code.Elem(Code.Expr.newVar(Code.Expr.newConst[S](code))))
        obj.attr.put(key, c)
        c
    }
    codeObj
  }

  // ---- frame impl ----

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