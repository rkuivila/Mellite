/*
 *  CodeViewImpl.scala
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

import de.sciss.desktop.UndoManager
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.mellite.gui.edit.CompoundEdit
import de.sciss.mellite.gui.impl.component.SpinningProgressBar
import de.sciss.scalainterpreter.{InterpreterPane, Interpreter, CodePane}
import scala.collection.mutable
import scala.concurrent.Future
import scala.swing.{FlowPanel, Component, Action, BorderPanel, Button, Swing}
import Swing._
import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.expr.ExprImplicits
import scala.util.Failure
import scala.util.Success
import de.sciss.syntaxpane.SyntaxDocument
import de.sciss.icons.raphael
import de.sciss.swingplus.Implicits._
import java.awt.Color
import javax.swing.Icon
import javax.swing.event.{UndoableEditEvent, UndoableEditListener}
import de.sciss.lucre.expr.{Expr, String => StringEx}
import java.beans.{PropertyChangeEvent, PropertyChangeListener}
import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx, requireEDT}
import javax.swing.undo.UndoableEdit
import de.sciss.synth.proc.Obj

object CodeViewImpl {
  private val intpMap = mutable.WeakHashMap.empty[Int, Future[Interpreter]]

  /* We use one shared interpreter for all code frames of each context. */
  private def interpreter(id: Int): Future[Interpreter] = {
    requireEDT()
    intpMap.getOrElse(id, {
      val cfg     = Interpreter.Config()
      cfg.imports = Code.getImports(id)
      val res     = Interpreter.async(cfg)
      intpMap.put(id, res)
      res
    })
  }

  def apply[S <: Sys[S]](obj: Obj.T[S, Code.Elem], code0: Code)
                        (handlerOpt: Option[CodeView.Handler[S, code0.In, code0.Out]])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S],
                         undoManager: UndoManager): CodeView[S] = {
    // val source0 = sourceCode.value
    // val objH        = tx.newHandle(obj)
    val codeEx      = obj.elem.peer
    val codeVarHOpt = codeEx match {
      case Expr.Var(vr) =>
        import Code.Expr.varSerializer
        Some(tx.newHandle(vr))
      case _            => None
    }
    // val code0   = codeEx.value
    // val source0 = code0.source
    // val sourceH = tx.newHandle(sourceCode)(StringEx.varSerializer[S])
    val res     = new Impl[S, code0.In, code0.Out](codeVarHOpt, code0)(handlerOpt)
    res.init()
    res
  }

  private final class Impl[S <: Sys[S], In0, Out0](codeVarHOpt: Option[stm.Source[S#Tx, Expr.Var[S, Code]]],
                                        private var code: Code { type In = In0; type Out = Out0 })(
                                        handlerOpt: Option[CodeView.Handler[S, In0, Out0]])
                                       (implicit undoManager: UndoManager, val workspace: Workspace[S],
                                        val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with CodeView[S] with ModelImpl[CodeView.Update] {

    // import ExecutionContext.Implicits.global

    private var _dirty = false
    def dirty = _dirty
    def dirty_=(value: Boolean): Unit = if (_dirty != value) {
      _dirty = value
      actionApply.enabled = value
      dispatch(CodeView.DirtyChange(value))
    }

    private type CodeT = Code { type In = In0; type Out = Out0 }

    private val codeCfg = {
      val b = CodePane.Config()
      b.text = code.source
      b.build
    }

    // import code.{id => codeID}

    private var codePane: CodePane = _
    private var futCompile = Option.empty[Future[Any]]
    private var actionApply: Action = _

    def isCompiling: Boolean = {
      requireEDT()
      futCompile.isDefined
    }

    protected def currentText: String = codePane.editor.getText

    def dispose()(implicit tx: S#Tx) = ()

    def undoAction: Action = Action.wrap(codePane.editor.getActionMap.get("undo"))
    def redoAction: Action = Action.wrap(codePane.editor.getActionMap.get("redo"))

    private def saveSource(newSource: String)(implicit tx: S#Tx): Option[UndoableEdit] = {
      val expr  = ExprImplicits[S]
      import expr._
      // import StringEx.{varSerializer, serializer}
      val imp = ExprImplicits[S]
      import imp._
      codeVarHOpt.map { source =>
        import Code.Expr.{serializer, varSerializer}
        val newCode = Code.Expr.newConst[S](code.updateSource(newSource))
        EditVar.Expr[S, Code]("Change Source Code", source(), newCode)
      }
    }

    private def addEditAndClear(edit: UndoableEdit): Unit = {
      requireEDT()
      undoManager.add(edit)
      // this doesn't work properly
      // component.setDirty(value = false) // do not erase undo history

      // so let's clear the undo history now...
      codePane.editor.getDocument.asInstanceOf[SyntaxDocument].clearUndos()
    }

    def save(): Future[Unit] = {
      requireEDT()
      val newCode = currentText
      if (handlerOpt.isDefined) {
        compileSource(newCode, save = true)
      } else {
        val editOpt = cursor.step { implicit tx =>
          saveSource(newCode)
        }
        editOpt.foreach(addEditAndClear)
        Future.successful[Unit] {}
      }
    }

    private def saveSourceAndObject(newCode: String, in: In0, out: Out0)(implicit tx: S#Tx): Option[UndoableEdit] = {
      val edit1 = saveSource(newCode)
      val edit2 = handlerOpt.map { handler =>
        handler.save(in, out)
      }
      val edits0  = edit2.toList
      val edits   = edit1.fold(edits0)(_ :: edits0)
      CompoundEdit(edits, "Save and Apply Code")
    }

    private def compile(): Unit = compileSource(currentText, save = false)

    private def compileSource(newCode: String, save: Boolean): Future[Unit] = {
      val saveObject = handlerOpt.isDefined && save
      if (futCompile.isDefined && !saveObject) return Future.successful[Unit] {}

      ggProgress          .spinning = true
      actionCompile       .enabled  = false
      if (saveObject) actionApply.enabled = false

      code = code.updateSource(newCode)

      val fut = handlerOpt match {
        case Some(handler) if save =>
          // val _fut = Library.compile(newCode)
          val _fut = Code.future {
            val in  = handler.in()
            val out = code.execute(in)
            (in, out)
          }
          _fut.onSuccess { case (in, out) =>
            defer {
              val editOpt = cursor.step { implicit tx =>
                saveSourceAndObject(newCode, in, out)
              }
              editOpt.foreach(addEditAndClear)
            }
          }
          _fut
        case _ =>
          code.compileBody()
      }

      futCompile = Some(fut)
      fut.onComplete { res =>
        defer {
          futCompile                    = None
          ggProgress          .spinning = false
          actionCompile       .enabled  = true
          if (saveObject) actionApply.enabled = true

          val iconColr = res match {
            case Success(_) =>
              clearGreen = true
              new Color(0x00, 0xC0, 0x00)                           // "\u2713"
            case Failure(Code.CompilationFailed()) => Color.red     // "error!"
            case Failure(Code.CodeIncomplete   ()) => Color.orange  // "incomplete!"
            case Failure(e) =>
              e.printStackTrace()
              Color.red
          }
          ggCompile.icon = compileIcon(Some(iconColr))
        }
      }
      fut.map(_ => ())
    }

    private lazy val ggProgress = new SpinningProgressBar

    private lazy val actionCompile = Action("Compile")(compile())

    private lazy val ggCompile: Button = GUI.toolButton(actionCompile, raphael.Shapes.Hammer,
      tooltip = "Verify that current buffer compiles")

    private def compileIcon(colr: Option[Color]): Icon =
      raphael.Icon(extent = 20, fill = colr.getOrElse(raphael.TexturePaint(24)),
        shadow = raphael.WhiteShadow)(raphael.Shapes.Hammer)

    private var clearGreen = false

    def init()(implicit tx: S#Tx): Unit = deferTx(guiInit())

    private def guiInit(): Unit = {
      codePane        = CodePane(codeCfg)
      val iPane       = InterpreterPane.wrapAsync(interpreter(code.id), codePane)

      actionApply = Action("Apply")(save())
      actionApply.enabled = false

      lazy val doc = codePane.editor.getDocument.asInstanceOf[SyntaxDocument]
      doc.addUndoableEditListener(
        new UndoableEditListener {
          def undoableEditHappened(e: UndoableEditEvent): Unit =
            if (clearGreen) {
              clearGreen = false
              ggCompile.icon = compileIcon(None)
            }
        }
      )

      doc.addPropertyChangeListener(SyntaxDocument.CAN_UNDO, new PropertyChangeListener {
        def propertyChange(e: PropertyChangeEvent): Unit = dirty = doc.canUndo
      })

      lazy val ggApply: Button = GUI.toolButton(actionApply, raphael.Shapes.Check , tooltip = "Save text changes")

      val panelBottom = new FlowPanel(FlowPanel.Alignment.Trailing)(
        HGlue, ggApply, ggCompile, ggProgress) // HStrut(16))

      val iPaneC  = iPane.component
      val iPaneCW = Component.wrap(iPaneC)
      val top     = iPaneC.getComponent(iPaneC.getComponentCount - 1) match {
        case jc: javax.swing.JComponent =>
          jc.add(panelBottom.peer)
          iPaneCW
        case _ => new BorderPanel {
          add(iPaneCW    , BorderPanel.Position.Center)
          add(panelBottom, BorderPanel.Position.South )
        }
      }

      component = top
      iPane.component.requestFocus()
    }
  }
}