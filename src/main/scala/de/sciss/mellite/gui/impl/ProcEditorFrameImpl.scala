/*
 *  ProcEditorFrameImpl.scala
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

import de.sciss.lucre.stm.{Source, Disposable, Cursor}
import de.sciss.synth.proc.{Obj, ExprImplicits, ProcKeys, Proc, StringElem}
import scala.swing.{Button, FlowPanel, Component, Label, TextField, BorderPanel, Action, Frame}
import javax.swing.WindowConstants
import java.awt.event.{WindowEvent, WindowAdapter}
import de.sciss.scalainterpreter.{Interpreter, CodePane}
import java.awt.Dimension
import de.sciss.synth.SynthGraph
import de.sciss.mellite.impl.InterpreterSingleton
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.swing.{defer, deferTx, requireEDT}

object ProcEditorFrameImpl {
  def apply[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])(implicit tx: S#Tx, cursor: Cursor[S]): ProcEditorFrame[S] = {
    val view = new Impl(/* cursor.position, */ tx.newHandle(proc), proc.toString()) {
      protected val observer = proc.changed.react { implicit tx => upd =>
        upd.changes.foreach {
          //               case Proc.Rename( Change( _, now )) =>
          //                  deferTx( name = now )
          case _ =>
        }
      }
    }

    val attr            = proc.attr
    val initName        = attr.expr[String](ProcKeys.attrName).fold("<unnamed>")(_.value)
    val initGraphSource = attr.expr[String](ProcKeys.attrGraphSource).map(_.value) // graph.source

    deferTx {
      view.guiInit(initName, initGraphSource)
    }
    view
  }

  private abstract class Impl[S <: Sys[S]](/* csrPos: S#Acc, */ procH: Source[S#Tx, Obj.T[S, Proc.Elem]], title: String)
                                          (protected implicit val cursor: Cursor[S])
    extends ProcEditorFrame[S] with ComponentHolder[Frame] with CursorHolder[S] {

    protected def observer: Disposable[S#Tx]

    private var ggName: TextField = _
    private var ggSource: CodePane = _
    private var intOpt = Option.empty[Interpreter]

    def name: String = {
      requireEDT()
      ggName.text
    }

    def name_=(value: String): Unit = {
      requireEDT()
      ggName.text = value
    }

    def proc(implicit tx: S#Tx): Obj.T[S, Proc.Elem] = procH() // tx.refresh( csrPos, staleProc )

    def dispose()(implicit tx: S#Tx): Unit = {
      observer.dispose()
      deferTx(component.dispose())
    }

    def guiInit(initName: String, initGraphSource: Option[String]): Unit = {
      val cCfg = CodePane.Config()
      initGraphSource.foreach(cCfg.text = _)
      ggSource = CodePane(cCfg)
      ggSource.editor.setEnabled(false)
      ggSource.editor.setPreferredSize(new Dimension(300, 300))
      InterpreterSingleton { in =>
        defer {
          intOpt = Some(in)
          ggSource.editor.setEnabled(true)
        }
      }

      val lbName = new Label("Name:")
      ggName = new TextField(initName, 16) {
        action = Action("") {
          // val newName = text
          println("NOT YET IMPLEMENTED")
          //               atomic { implicit tx =>
          //                  val imp  = ExprImplicits[ S ]
          //                  import imp._
          //                  proc.name_=( newName )
          //               }
        }
      }

      val lbStatus = new Label
      lbStatus.preferredSize = new Dimension(200, lbStatus.preferredSize.height)

      val ggCommit = Button("Commit") {
        intOpt.foreach { in =>
          val code = ggSource.editor.getText
          if (code != "") {
            val wrapped = /* InterpreterSingleton.wrap( */ "SynthGraph {\n" + code + "\n}" /* ) */
            val intRes = in.interpret(wrapped)
            //println( "Interpreter done " + intRes )
            intRes match {
              case Interpreter.Success(name, value) =>
                //                        val value = InterpreterSingleton.Result.value
                //println( "Success " + name + " value is " + value )
                value match {
                  case sg: SynthGraph =>
                    //println( "Success " + name + " is a graph" )
                    lbStatus.text = "Ok."
                    atomic { implicit tx =>
                      val imp = ExprImplicits[S]
                      import imp._
                      proc.attr.put(ProcKeys.attrGraphSource, StringElem(code)) // XXX TODO: should update the var
                      proc.elem.peer.graph() = sg
                    }
                  case _ =>
                    lbStatus.text = s"! Invalid result: $value !"
                }
              case Interpreter.Error(_) =>
                lbStatus.text = "! Error !"
              case Interpreter.Incomplete =>
                lbStatus.text = "! Code incomplete !"
            }
          }
        }
      }

      val topPanel = new FlowPanel(lbName, ggName)
      val botPanel = new FlowPanel(ggCommit, lbStatus)

      component = new Frame {
        title = "Process : " + title // staleProc
        // f*** scala swing... how should this be written?
        peer.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE)
        peer.addWindowListener(new WindowAdapter {
          override def windowClosing(e: WindowEvent): Unit =
            atomic(implicit tx => dispose())
        })

        contents = new BorderPanel {
          import BorderPanel.Position._
          add(topPanel, North)
          add(Component.wrap(ggSource.component), Center)
          add(botPanel, South)
        }
        pack()
        centerOnScreen()
        open()
      }
    }
  }
}
