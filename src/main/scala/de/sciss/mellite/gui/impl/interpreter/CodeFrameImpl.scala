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
import de.sciss.scalainterpreter.CodePane
import scala.swing.{FlowPanel, BorderPanel, Swing, Label, Button, Component}
import de.sciss.lucre.stm
import de.sciss.lucre.expr.Expr
import scala.concurrent.Future
import Swing._
import scala.util.{Failure, Success}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder

object CodeFrameImpl {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Code[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): CodeFrame[S] = {
    val _name = elem.name.value
    val _code = elem.entity.value

    new Impl[S] {
      val document = doc
      protected val name      = _name
      protected val contextName = _code.contextName
      protected val _cursor   = cursor
      protected val codeH     = tx.newHandle(elem.entity)(Codes.serializer)
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

      deferTx(guiInit())
    }
  }

  private abstract class Impl[S <: Sys[S]] extends CodeFrame[S] with ComponentHolder[Window] {
    // protected def intpCfg: Interpreter.Config
    protected def codeCfg: CodePane.Config
    protected def name: String
    protected def contextName: String
    protected def _cursor: stm.Cursor[S]
    protected def codeH: stm.Source[S#Tx, Expr[S, Code]]
    protected def codeID: Int

    private var codePane: CodePane        = _
    // private var intp    : Interpreter     = _
    // private var intpPane: InterpreterPane = _
    private var futCompile = Option.empty[Future[Unit]]
    private var ggStatus: Label = _

    private def currentText: String = codePane.editor.getText

    private def checkClose(): Unit = {
      if (futCompile.isDefined) {
        ggStatus.text = "busy!"
        return
      }

      val newText = currentText
      if (newText != codeCfg.text && newText.stripMargin != "") {
        val message = "The code has been edited.\nDo you want to save the changes?"
        val opt = OptionPane.confirmation(message = message, optionType = OptionPane.Options.YesNoCancel,
          messageType = OptionPane.Message.Warning)
        opt.title = s"Close Code Editor - $name"
        opt.show(Some(component)) match {
          case OptionPane.Result.No =>
          case OptionPane.Result.Yes =>
            _cursor.step { implicit tx =>
              codeH() match {
                case Expr.Var(vr) => vr() = Codes.newConst(Code(codeID, newText))
              }
            }

          case OptionPane.Result.Cancel | OptionPane.Result.Closed =>
            return
        }
      }
      disposeFromGUI()
    }

    private def disposeFromGUI(): Unit = {
      _cursor.step { implicit tx =>
        disposeData()
      }
      component.dispose()
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      deferTx {
        component.dispose()
        // intp.dispose()
      }
    }

    private def disposeData()(implicit tx: S#Tx): Unit = {
      // observer.dispose()
    }

    def guiInit(): Unit = {
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

      component = new WindowImpl {
        frame =>

        override def style = Window.Auxiliary

        title           = s"$name : $contextName Code"
        contents        = new BorderPanel {
          add(Component.wrap(codePane.component), BorderPanel.Position.Center)
          add(panelBottom, BorderPanel.Position.South)
        }
        closeOperation  = Window.CloseIgnore

        reactions += {
          case Window.Closing(_) =>
            checkClose()
        }

        pack()
        GUI.centerOnScreen(this)
        front()
      }
    }
  }
}