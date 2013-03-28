/*
 *  DocumentFrameImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import swing.{Swing, TextField, Alignment, Label, Dialog, MenuItem, Component, Orientation, SplitPane, FlowPanel, Action, Button, BorderPanel, Frame}
import de.sciss.lucre.stm.{Cursor, Disposable}
import de.sciss.synth.proc.Sys
import de.sciss.scalainterpreter.{Interpreter, InterpreterPane}
import Swing._
import scalaswingcontrib.group.GroupPanel
import de.sciss.synth.expr.Strings
import tools.nsc.interpreter.NamedParam
import de.sciss.desktop
import desktop.{Window, Menu}
import scalaswingcontrib.PopupMenu
import de.sciss.desktop.impl.WindowImpl

object DocumentFrameImpl {
  def apply[S <: Sys[S]](doc: Document[S])(implicit tx: S#Tx): DocumentFrame[S] = {
    implicit val csr  = doc.cursor
    val groupView     = GroupView(doc.elements)
    val view          = new Impl(doc, groupView)
    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val document: Document[S], groupView: GroupView[S])
    extends DocumentFrame[S] with ComponentHolder[Window] with CursorHolder[S] {

    protected implicit def cursor: Cursor[S] = document.cursor

    private def transport(implicit tx: S#Tx): Option[Document.Transport[S]] = {
      None
      //         for( gl <- groupsView.list; gidx <- groupsView.guiSelection.headOption; group <- gl.get( gidx );
      //              tl <- transpView.list; tidx <- transpView.guiSelection.headOption; transp <- document.transports( group ).get( tidx ))
      //            yield transp
    }

    private def actionAddFolder() {
      val res = Dialog.showInput[String](groupView.component, "Enter initial folder name:", "New Folder", Dialog.Message.Question, initial = "Folder")
      res.foreach { name =>
        atomic { implicit tx =>
          val parent  = document.elements
          parent.addLast(Element.Group(name, Elements[S]))
        }
      }
    }

    private def actionAddInt() {
      println("actionAddInt")
    }

    private def actionAddDouble() {
      println("actionAddDouble")
    }

    private def actionAddString() {
      val ggName  = new TextField(10)
      val ggValue = new TextField(20)
      ggName.text   = "String"
      ggValue.text  = "Value"

      import language.reflectiveCalls // why does GroupPanel need reflective calls?
      import desktop.Implicits._
      val box = new GroupPanel {
        val lbName  = new Label("Name:",  EmptyIcon, Alignment.Right)
        val lbValue = new Label("Value:", EmptyIcon, Alignment.Right).initialDialogFocus()
        theHorizontalLayout is Sequential(Parallel(Trailing)(lbName, lbValue), Parallel(ggName, ggValue))
        theVerticalLayout   is Sequential(Parallel(Baseline)(lbName, ggName), Parallel(Baseline)(lbValue, ggValue))
      }

      val res = Dialog.showConfirmation(groupView.component, box.peer, "New String", Dialog.Options.OkCancel,
        Dialog.Message.Question)

      if (res == Dialog.Result.Ok) {
        // println(s"name = ${ggName.text} ; value = ${ggValue.text}")
        val name    = ggName.text
        val value   = ggValue.text
        atomic { implicit tx =>
          val parent  = document.elements
          parent.addLast(Element.String(name = name, init = Strings.newConst(value)))
        }
      }
    }

    private def actionAddTimeline() {
      println("actionAddTimeline")
    }

    def guiInit() {
      requireEDT()
      require(comp == null, "Initialization called twice")

      lazy val addPopup: PopupMenu = {
        import Menu._
        val pop = Popup()
          .add(Item("folder", Action("Folder")(actionAddFolder())))
          .add(Item("string", Action("String")(actionAddString())))
          .add(Item("int",    Action("Int"   )(actionAddInt   ())))
          .add(Item("double", Action("Double")(actionAddDouble())))
        pop.create(frame)
      }

      lazy val ggAddGroup: Button = Button("+") {
        val bp = ggAddGroup
        addPopup.show(bp, bp.size.width, bp.size.height)

        //        atomic { implicit tx =>
        ////          implicit val spans = SpanLikes
        ////          val group = ProcGroup_.Modifiable[S]
        ////          document.groups.addLast(group)
        //        }
      }
      ggAddGroup.peer.putClientProperty("JButton.buttonType", "roundRect")

      def mkDelButton[Elem <: Disposable[S#Tx], U](view: GroupView[S]): Button =
        new Button(Action("\u2212") {
          //          val indices = view.guiSelection
          //          if (indices.nonEmpty) atomic {
          //            implicit tx =>
          //              view.list.flatMap(_.modifiableOption).foreach {
          //                ll =>
          //                  val sz = ll.size
          //                  val ind1 = indices.filter(_ < sz).sortBy(-_)
          //                  ind1.foreach {
          //                    idx =>
          //                      ll.removeAt(idx).dispose()
          //                  }
          //              }
          //          }
        }) {
          enabled = false
          peer.putClientProperty("JButton.buttonType", "roundRect")
        }

      lazy val ggDelGroup = mkDelButton(groupView)

      lazy val ggViewTimeline = new Button(Action("View Timeline") {

      }) {
        enabled = false
      }

      lazy val groupsButPanel = new FlowPanel( ggAddGroup, ggDelGroup, ggViewTimeline )

      lazy val ggAddTransp = new Button(Action("+") {
        //            atomic { implicit tx =>
        //               for( ll <- groupsView.list; idx <- groupsView.guiSelection.headOption; group <- ll.get( idx )) {
        //                  val transp = Transport( group )
        //                  document.transports( group ).addLast( transp )
        //               }
        //            }
      }) {
        enabled = false
      }

      //         val ggDelTransp = mkDelButton( transpView )
      //
      //         val ggViewInstant = new Button( Action( "View Instant" ) {
      //            atomic { implicit tx =>
      //               for( gl <- groupsView.list; gidx <- groupsView.guiSelection.headOption; group <- gl.get( gidx );
      //                    tidx <- transpView.guiSelection.headOption;
      //                    transp <- document.transports( group ).get( tidx )) {
      //
      //                  InstantGroupFrame( group, transp )
      //               }
      //            }
      //         }) {
      //            enabled = false
      //         }
      //
      //         val transpButPanel = new FlowPanel( ggAddTransp, ggDelTransp, ggViewInstant )

      lazy val groupsPanel = new BorderPanel {
        add(groupView.component, BorderPanel.Position.Center)
        add(groupsButPanel, BorderPanel.Position.South)
      }

      //         val transpPanel = new BorderPanel {
      //            add( transpView.component, BorderPanel.Position.Center )
      //            add( transpButPanel, BorderPanel.Position.South )
      //         }

      //         groupView.guiReact {
      //            case ListView.SelectionChanged( indices ) =>
      ////               println( "SELECTION " + indices )
      //               val isSelected = indices.nonEmpty
      //               ggDelGroup.enabled      = isSelected
      //               ggViewTimeline.enabled  = isSelected
      //               val isSingle = indices.size == 1
      //               ggAddTransp.enabled = isSingle
      ////               ggDelTransp.enabled = false
      //
      ////               atomic { implicit tx =>
      ////                  val transpList = if( isSingle ) {
      ////                     for( ll <- groupsView.list; idx <- indices.headOption; group <- ll.get( idx ))
      ////                        yield document.transports( group )
      ////                  } else {
      ////                     None
      ////                  }
      ////                  transpView.list_=( transpList )
      ////               }
      //         }

      //         transpView.guiReact {
      //            case ListView.SelectionChanged( indices ) =>
      //               val isSelected          = indices.nonEmpty
      //               ggDelTransp.enabled     = isSelected
      //               ggViewInstant.enabled   = isSelected
      //         }
      //

      //         val ggAural = Button( "Aural" ) {
      //            atomic { implicit tx =>
      //               transport.foreach { t =>
      ////                  val aural: AuralSystem[ S ] = ???
      ////                  AuralPresentation.run( t, aural )
      //               }
      //            }
      //         }

      lazy val ggTest = Button("ELEMENTS") {
        atomic {
          implicit tx =>
            println("ELEMENTS : " + document.elements.iterator.toIndexedSeq)
        }
      }

      lazy val intp = {
        //          val config          = InterpreterPane.Config()
        val intpConfig = Interpreter.Config()
        //          intpConfig.executor = "de.sciss.mellite.InterpreterContext"
        intpConfig.imports = Seq("de.sciss.mellite._", "de.sciss.synth._", "proc._", "ugen._")
        import document.systemType
        intpConfig.bindings = Seq(NamedParam[Document[S]]("doc", document))
        //          intpConfig.out      = ???
        //          val codeConfig      = CodePane.Config()
        InterpreterPane(interpreterConfig = intpConfig)
      }

      lazy val splitPane = new SplitPane(Orientation.Horizontal, groupsPanel, Component.wrap(intp.component))

      lazy val frame: Window = new WindowImpl {
        def style = Window.Regular
        def handler = Mellite.windowHandler
        title = "Document : " + document.folder.getName
        closeOperation = Window.CloseIgnore
        contents = new BorderPanel {
          add(splitPane, BorderPanel.Position.Center)
          //add( groupsPanel, BorderPanel.Position.Center )
          add(ggTest, BorderPanel.Position.South)
        }
        pack()
        // centerOnScreen()
        front()
      }

      comp = frame
    }
  }
}
