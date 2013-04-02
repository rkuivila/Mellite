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

import swing.{Swing, TextField, Alignment, Label, Dialog, Component, Orientation, SplitPane, FlowPanel, Action, Button, BorderPanel}
import de.sciss.lucre.stm.Cursor
import de.sciss.synth.proc.{Artifact, Grapheme, ProcGroup, Sys}
import de.sciss.scalainterpreter.{Interpreter, InterpreterPane}
import Swing._
import scalaswingcontrib.group.GroupPanel
import de.sciss.synth.expr.{Doubles, Longs, Strings}
import tools.nsc.interpreter.NamedParam
import de.sciss.desktop
import desktop.Window.Style
import desktop.{DialogSource, OptionPane, Window, Menu}
import scalaswingcontrib.PopupMenu
import desktop.impl.WindowImpl
import de.sciss.synth.io.AudioFile

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

    private def addElement(elem: Element[S])(implicit tx: S#Tx) {
      val sel = groupView.selection
      val parent = if (sel.isEmpty) document.elements else sel.head match {
        case (_, _parent: ElementView.Group[S]) => _parent.group
        case (_ :+ _parent, _)                  => _parent.group
        case _                                  => document.elements
      }
      parent.addLast(elem)
    }

    private def actionAddFolder() {
      val res = Dialog.showInput[String](groupView.component, "Enter initial folder name:", "New Folder",
        Dialog.Message.Question, initial = "Folder")
      res.foreach { name =>
        atomic { implicit tx =>
          addElement(Element.Group(name, Elements[S]))
        }
      }
    }

    private def actionAddProcGroup() {
      val res = Dialog.showInput[String](groupView.component, "Enter initial group name:", "New Proc Group",
        Dialog.Message.Question, initial = "Timeline")
      res.foreach { name =>
        atomic { implicit tx =>
          addElement(Element.ProcGroup(name, ProcGroup.Modifiable[S]))
        }
      }
    }

    private def actionAddAudioFile() {
//      val res = Dialog.showInput[String](groupView.component, "Enter initial group name:", "New Proc Group",
//        Dialog.Message.Question, initial = "Timeline")
//      res.foreach { name =>
//        atomic { implicit tx =>
//          addElement(Element.ProcGroup(name, ProcGroup.Modifiable[S]))
//        }
//      }
      val res = FileDialog.open(None/* Some(frame) */, None, None, "Add Audio File", AudioFile.identify(_).isDefined)
      res.foreach { f =>
        val spec      = AudioFile.readSpec(f)
        val name0     = f.getName
        val i         = name0.lastIndexOf('.')
        val name      = if (i < 0) name0 else name0.substring(0, i)
        val offset    = Longs.newConst[S](0L)
        val gain      = Doubles.newConst[S](1.0)
        val artifact  = Artifact(f.getAbsolutePath)
        atomic { implicit tx =>
          val audio = Grapheme.Elem.Audio(artifact, spec, offset, gain)
          addElement(Element.AudioGrapheme(name, audio))
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
      // import desktop.Implicits._
      val box = new GroupPanel {
        val lbName  = new Label( "Name:", EmptyIcon, Alignment.Right)
        val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
        theHorizontalLayout is Sequential(Parallel(Trailing)(lbName, lbValue), Parallel(ggName, ggValue))
        theVerticalLayout   is Sequential(Parallel(Baseline)(lbName, ggName ), Parallel(Baseline)(lbValue, ggValue))
      }

      val pane = OptionPane.confirmation(box, optionType = Dialog.Options.OkCancel,
        messageType = Dialog.Message.Question, focus = Some(ggValue))
      val res = frame.show(pane -> "New String")

      if (res == Dialog.Result.Ok) {
        // println(s"name = ${ggName.text} ; value = ${ggValue.text}")
        val name    = ggName.text
        val value   = ggValue.text
        atomic { implicit tx =>
          addElement(Element.String(name = name, init = Strings.newConst(value)))
        }
      }
    }

    var frame: Frame[S] = null

    def guiInit() {
      requireEDT()
      require(comp == null, "Initialization called twice")

      lazy val addPopup: PopupMenu = {
        import Menu._
        val pop = Popup()
          .add(Item("folder",     Action("Folder"    )(actionAddFolder   ())))
          .add(Item("timeline",   Action("Timeline"  )(actionAddProcGroup())))
          .add(Item("audiofile",  Action("Audio File")(actionAddAudioFile())))
          .add(Item("string",     Action("String"    )(actionAddString   ())))
          .add(Item("int",        Action("Int"       )(actionAddInt      ())))
          .add(Item("double",     Action("Double"    )(actionAddDouble   ())))
        val res = pop.create(frame)
        res.peer.pack() // so we can read `size` correctly
        res
      }

      lazy val ggAdd: Button = Button("+") {
        val bp = ggAdd
        addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
      }
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggDelete: Button = Button("\u2212") {
        val views = groupView.selection.map { case (p, view) => (p.last, view) }
        if (views.nonEmpty) atomic { implicit tx =>
          views.foreach { case (parent, child) =>
            parent.group.remove(child.element())
          }
        }
      }
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggView: Button = Button("View") {
        val views = groupView.selection.map { case (_, view) => view }
        if (views.nonEmpty) atomic { implicit tx =>
          views.foreach { view =>
            view.element() match {
              case e: Element.ProcGroup[S] =>
                val tlv = TimelineView(e)
                new WindowImpl {
                  def handler = Mellite.windowHandler
                  def style   = Window.Regular
                  contents    = tlv.component
                  pack()
                  // centerOnScreen()
                  front()
                }
              case _ => // ...
            }
          }
        }
      }
      ggView.enabled = false
      ggView.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val groupsButPanel = new FlowPanel(ggAdd, ggDelete, ggView)

      lazy val groupsPanel = new BorderPanel {
        add(groupView.component, BorderPanel.Position.Center)
        add(groupsButPanel,      BorderPanel.Position.South )
      }

      lazy val intp = {
        val intpConfig = Interpreter.Config()
        intpConfig.imports = Seq("de.sciss.mellite._", "de.sciss.synth._", "proc._", "ugen._")
        import document.systemType
        intpConfig.bindings = Seq(NamedParam[Document[S]]("doc", document))
        InterpreterPane(interpreterConfig = intpConfig)
      }

      lazy val splitPane = new SplitPane(Orientation.Horizontal, groupsPanel, Component.wrap(intp.component))

      frame = new Frame(document, new BorderPanel {
        add(splitPane, BorderPanel.Position.Center)
      })

      groupView.addListener {
        case GroupView.SelectionChanged(_, sel) =>
          ggAdd   .enabled  = sel.size < 2
          ggDelete.enabled  = sel.nonEmpty
          ggView  .enabled  = sel.nonEmpty
      }

      comp = frame
    }
  }

  private final class Frame[S <: Sys[S]](document: Document[S], _contents: Component) extends WindowImpl {
    def style = Window.Regular
    def handler = Mellite.windowHandler
    title = "Document : " + document.folder.getName
    closeOperation = Window.CloseIgnore
    contents = _contents
    pack()
    // centerOnScreen()
    front()

    def show[A](source: DialogSource[A]): A = showDialog(source)
  }
}
