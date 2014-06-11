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

import de.sciss.desktop.{OptionPane, Window}
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.scalainterpreter.CodePane
import scala.swing.{FlowPanel, BorderPanel, Swing, Label, Button, Component}
import de.sciss.lucre.stm
import de.sciss.lucre.expr.Expr
import scala.concurrent.Future
import Swing._
import scala.util.{Failure, Success}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.{deferTx, defer}
import de.sciss.synth.proc
import proc.Implicits._
import de.sciss.synth.proc.Obj

object CodeFrameImpl {
  def apply[S <: Sys[S]](doc: Workspace[S], obj: Obj.T[S, Code.Elem])
                        (implicit tx: S#Tx, _cursor: stm.Cursor[S]): CodeFrame[S] = {
    val _name   = obj.attr.name
    val _codeEx = obj.elem.peer
    val _code   = _codeEx.value

    val view = new ViewImpl[S] {
      val workspace = doc
      protected val name      = _name
      protected val contextName = _code.contextName
      val cursor   = _cursor
      protected val codeH     = tx.newHandle(_codeEx)(Codes.serializer[S])
      protected val codeID    = _code.id

      protected val codeCfg = {
        val b = CodePane.Config()
        b.text = _code.source
        b.build
      }
      //      protected val intpCfg = {
      //        val b = Interpreter.Config()
      //        b.imports = Seq(
      //          "de.sciss.mellite._",
      //          "de.sciss.synth._",
      //          "Ops._",
      //          "concurrent.duration._",
      //          "gui.InterpreterFrame.Bindings._"
      //        )
      //        b.build
      //      }

    }
    view.init()
    val res = new FrameImpl(view, title0 = s"${_name} : ${_code.contextName} Code")
    res.init()
    res
  }

  private abstract class ViewImpl[S <: Sys[S]]
    extends ViewHasWorkspace[S] with ComponentHolder[Component] {

    protected def codeCfg: CodePane.Config
    protected def name: String
    protected def contextName: String
    // protected def _cursor: stm.Cursor[S]
    protected def codeH: stm.Source[S#Tx, Expr[S, Code]]
    protected def codeID: Int

    private var codePane: CodePane        = _
    private var codePaneC: Component = _
    private var futCompile = Option.empty[Future[Unit]]
    private var ggStatus: Label = _

    private def currentText: String = codePane.editor.getText

    def init()(implicit tx: S#Tx): Unit = deferTx(guiInit())

    def dispose()(implicit tx: S#Tx): Unit = ()

    private def guiInit(): Unit = {
      codePane  = CodePane(codeCfg)
      // intp      = Interpreter(intpCfg)
      // intpPane  = InterpreterPane.wrap(intp, codePane)

      ggStatus  = new Label(null)

      val ggCompile = Button("Compile") {
        if (futCompile.isDefined) {
          ggStatus.text = "busy!"
          return
        }
        ggStatus.text = "..."
        val newCode = Code(codeID, currentText)
        val fut     = newCode.compileBody()
        futCompile  = Some(fut)
        fut.onComplete { res =>
          defer {
            futCompile = None
            val st = res match {
              case Success(_) => "\u2713"
              case Failure(Code.CompilationFailed()) =>
                "error!"
              case Failure(Code.CodeIncomplete()) =>
                "incomplete!"
              case Failure(e) =>
                e.printStackTrace()
                "error!"
            }
            ggStatus.text = st
          }
        }
      }
      GUI.round(ggCompile)

      val panelBottom = new FlowPanel(FlowPanel.Alignment.Trailing)(HGlue, ggStatus, ggCompile, HStrut(16))

      codePaneC = Component.wrap(codePane.component)

      component = new BorderPanel {
        add(codePaneC, BorderPanel.Position.Center)
        add(panelBottom, BorderPanel.Position.South)
      }
    }

    def checkClose(): Boolean = {
      if (futCompile.isDefined) {
        ggStatus.text = "busy!"
        false
      }

      val newText = currentText
      (newText == codeCfg.text || newText.stripMargin.isEmpty) || {
        val message = "The code has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close Code Editor - $name"
        opt.show(GUI.findWindow(component)) match {
          case OptionPane.Result.No => true
          case OptionPane.Result.Yes =>
            cursor.step { implicit tx =>
              codeH() match {
                case Expr.Var(vr) => vr() = Codes.newConst[S](Code(codeID, newText))
              }
            }
            true

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            false
        }
      }
    }
  }

  private final class FrameImpl[S <: Sys[S]](val view: ViewImpl[S], title0: String)
    extends WindowImpl[S](title0) with CodeFrame[S] {

    override protected def checkClose(): Boolean = view.checkClose()

    override def style = Window.Auxiliary
  }
}