/*
 *  ElementsFrameImpl.scala
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
package document

import scala.swing.{ComboBox, TextField, Dialog, Component, FlowPanel, Action, Button, BorderPanel}
import de.sciss.lucre.stm
import de.sciss.synth.proc.{ProcGroup, Sys}
import de.sciss.synth.expr._
import de.sciss.desktop.{FileDialog, DialogSource, Window, Menu}
import scalaswingcontrib.PopupMenu
import de.sciss.desktop.impl.WindowImpl
import de.sciss.synth.io.AudioFile
import javax.swing.SpinnerNumberModel
import de.sciss.file._
import de.sciss.swingplus.Spinner

object ElementsFrameImpl {
  def apply[S <: Sys[S]](doc: Document[S])(implicit tx: S#Tx,
                                           cursor: stm.Cursor[S]): DocumentElementsFrame[S] = {
    // implicit val csr  = doc.cursor
    val folderView      = FolderView(doc, doc.elements)
    val view            = new Impl(doc, folderView)

    guiFromTx {
      view.guiInit()
    }
    view
  }

  private final class Impl[S <: Sys[S]](val document: Document[S], folderView: FolderView[S])
                                       (implicit val cursor: stm.Cursor[S])
    extends DocumentElementsFrame[S] with ComponentHolder[Frame[S]] with CursorHolder[S] {

    // protected implicit def cursor: Cursor[S] = document.cursor

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      guiFromTx(comp.dispose())
    }

    private def disposeData()(implicit tx: S#Tx): Unit =
      folderView.dispose()

    def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    private def targetFolder(implicit tx: S#Tx): Folder[S] = {
      val sel = folderView.selection
      if (sel.isEmpty) document.elements else sel.head match {
        case (_, _parent: ElementView.Folder[S])        => _parent.folder
        case (_ :+ (_parent: ElementView.Folder[S]), _) => _parent.folder
        case _                                          => document.elements
      }
    }

    private def addElement(elem: Element[S])(implicit tx: S#Tx): Unit = {
      val parent = targetFolder
      parent.addLast(elem)
    }

    private def actionAddFolder(): Unit = {
      val res = Dialog.showInput[String](folderView.component, "Enter initial folder name:", "New Folder",
        Dialog.Message.Question, initial = "Folder")
      res.foreach { name =>
        atomic { implicit tx =>
          addElement(Element.Folder(name, Folder[S]))
        }
      }
    }

    private def actionAddProcGroup(): Unit = {
      val res = Dialog.showInput[String](folderView.component, "Enter initial group name:", "New ProcGroup",
        Dialog.Message.Question, initial = "Timeline")
      res.foreach { name =>
        atomic { implicit tx =>
          addElement(Element.ProcGroup(name, ProcGroup.Modifiable[S]))
        }
      }
    }

    private def actionAddArtifactLocation(): Unit = {
      val query = ActionArtifactLocation.queryNew(window = Some(comp))
      query.foreach { case (directory, name) =>
        atomic { implicit tx =>
          ActionArtifactLocation.create(directory, name, targetFolder)
        }
      }
    }

    private def actionAddAudioFile(): Unit = {
      val locViews  = folderView.locations
      val dlg       = FileDialog.open(init = locViews.headOption.map(_.directory), title = "Add Audio File")
      dlg.setFilter(AudioFile.identify(_).isDefined)
      dlg.show(None).foreach { f =>
        val spec          = AudioFile.readSpec(f)
        val locSourceOpt  = folderView.findLocation(f)
        locSourceOpt.foreach { locSource =>
          atomic { implicit tx =>
            val loc = locSource()
            loc.entity.modifiableOption.foreach { locM =>
              ElementActions.addAudioFile(targetFolder, -1, locM, f, spec)
            }
          }
        }
      }
    }

    private def actionAddInt(): Unit = {
      val expr      = ExprImplicits[S]
      import expr._
      val model     = new SpinnerNumberModel(0, Int.MinValue, Int.MaxValue, 1)
      val ggValue   = new Spinner(model)
      actionAddPrimitive(tpe = "Integer", ggValue = ggValue, prepare = Some(model.getNumber.intValue())) {
        implicit tx =>
          (name, value) => Element.Int(name, Ints.newVar(value))
      }
    }

    private def actionAddDouble(): Unit = {
      val expr      = ExprImplicits[S]
      import expr._
      val model     = new SpinnerNumberModel(0.0, Double.NegativeInfinity, Double.PositiveInfinity, 1.0)
      val ggValue   = new Spinner(model)
      actionAddPrimitive(tpe = "Double", ggValue = ggValue, prepare = Some(model.getNumber.doubleValue)) {
        implicit tx =>
          (name, value) => Element.Double(name, Doubles.newVar(value))
      }
    }

    private def actionAddString(): Unit = {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      actionAddPrimitive(tpe = "String", ggValue = ggValue, prepare = Some(ggValue.text)){ implicit tx =>
        (name, value) => Element.String(name, Strings.newVar(value))
      }
    }

    private def actionAddCode(): Unit = {
      val ggValue = new ComboBox(Seq(Code.FileTransform.name, Code.SynthGraph.name))
      actionAddPrimitive(tpe = "Code", ggValue = ggValue, prepare = ggValue.selection.index match {
        case 0 => Some(Code.FileTransform(
          """|val ain   = AudioFile.openRead(in)
             |val aout  = AudioFile.openWrite(out, ain.spec)
             |val bufSz = 8192
             |val buf   = ain.buffer(bufSz)
             |var rem   = ain.numFrames
             |while (rem > 0) {
             |  val chunk = math.min(bufSz, rem).toInt
             |  ain.read(buf, 0, chunk)
             |  // ...
             |  aout.write(buf, 0, chunk)
             |  rem -= chunk
             |  // checkAbort()
             |}
             |aout.close()
             |ain .close()
             |""".stripMargin))

        case 1 => Some(Code.SynthGraph(
          """|val in   = scan.In("in").ar
             |val sig  = in
             |scan.Out("out") := sig
             |""".stripMargin
        ))

        case _  => None
      }) { implicit tx =>
        (name, value) => Element.Code(name, Codes.newVar(Codes.newConst(value)))
      }
    }

    private def actionAddPrimitive[A](tpe: String, ggValue: Component, prepare: => Option[A])
                                     (create: S#Tx => (String, A) => Element[S]) {
      val nameOpt = GUI.keyValueDialog(value = ggValue, title = s"New $tpe", defaultName = tpe, window = Some(comp))
      nameOpt.foreach { name =>
        prepare.foreach { value =>
          atomic { implicit tx =>
            addElement(create(tx)(name, value))
          }
        }
      }
    }

    def guiInit(): Unit = {
      requireEDT()
      require(comp == null, "Initialization called twice")

      lazy val addPopup: PopupMenu = {
        import Menu._
        val pop = Popup()
          .add(Item("folder",       Action("Folder"       )(actionAddFolder          ())))
          .add(Item("procgroup",    Action("ProcGroup"    )(actionAddProcGroup       ())))
          .add(Item("artifactstore",Action("ArtifactStore")(actionAddArtifactLocation())))
          .add(Item("audiofile",    Action("Audio File"   )(actionAddAudioFile       ())))
          .add(Item("string",       Action("String"       )(actionAddString          ())))
          .add(Item("int",          Action("Int"          )(actionAddInt             ())))
          .add(Item("double",       Action("Double"       )(actionAddDouble          ())))
          .add(Item("code",         Action("Code"         )(actionAddCode            ())))
        val res = pop.create(comp)
        res.peer.pack() // so we can read `size` correctly
        res
      }

      lazy val ggAdd: Button = Button("+") {
        val bp = ggAdd
        addPopup.show(bp, (bp.size.width - addPopup.size.width) >> 1, bp.size.height - 4)
      }
      ggAdd.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggDelete: Button = Button("\u2212") {
        val views = folderView.selection.map { case (p, view) => (p.last, view) }
        if (views.nonEmpty) atomic { implicit tx =>
          views.foreach {
            case (parent: ElementView.FolderLike[S], child) =>
              parent.folder.remove(child.element())
            case _ =>
          }
        }
      }
      ggDelete.enabled = false
      ggDelete.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val ggView: Button = Button("View") {
        val views = folderView.selection.map { case (_, view) => view }
        if (views.nonEmpty) atomic { implicit tx =>
          import Mellite.auralSystem
          views.foreach {
            case view: ElementView.ProcGroup[S] =>
              // val e   = view.element()
              // import document.inMemory
              TimelineFrame(document, view.element())

            case view: ElementView.AudioGrapheme[S] =>
              AudioFileFrame(document, view.element())

            case view: ElementView.Recursion[S] =>
              RecursionFrame(document, view.element())

            case view: ElementView.Code[S] =>
              CodeFrame(document, view.element())

            case _ => // ...
          }
        }
      }
      ggView.enabled = false
      ggView.peer.putClientProperty("JButton.buttonType", "roundRect")

      lazy val folderButPanel = new FlowPanel(ggAdd, ggDelete, ggView)

      lazy val folderPanel = new BorderPanel {
        add(folderView.component, BorderPanel.Position.Center)
        add(folderButPanel,       BorderPanel.Position.South )
      }

      //      lazy val intp = {
      //        val intpConfig = Interpreter.Config()
      //        intpConfig.imports = Seq("de.sciss.mellite._", "de.sciss.synth._", "proc._", "ugen._")
      //        import document.systemType
      //        intpConfig.bindings = Seq(NamedParam[Document[S]]("doc", document))
      //        InterpreterPane(interpreterConfig = intpConfig)
      //      }

      // lazy val splitPane = new SplitPane(Orientation.Horizontal, folderPanel, Component.wrap(intp.component))

      comp = new Frame(this,
        folderPanel
        //        new BorderPanel {
        //        //        add(splitPane, BorderPanel.Position.Center)
        //        add(folderPanel,                BorderPanel.Position.Center)
        //        add(Component.wrap(serverPane), BorderPanel.Position.South )
        //        }
      )

      folderView.addListener {
        case FolderView.SelectionChanged(_, sel) =>
          ggAdd   .enabled  = sel.size < 2
          ggDelete.enabled  = sel.nonEmpty
          ggView  .enabled  = sel.nonEmpty
      }
    }
  }

  private final class Frame[S <: Sys[S]](view: Impl[S], _contents: Component) extends WindowImpl {
    def style       = Window.Regular
    def handler     = Mellite.windowHandler

    title           = s"${view.document.folder.base} : Elements"
    file            = Some(view.document.folder)
    closeOperation  = Window.CloseDispose
    reactions += {
      case Window.Closing(_) => view.frameClosing()
    }

    contents        = _contents

    pack()
    // centerOnScreen()
    GUI.placeWindow(this, 0.5f, 0f, 24)
    front()

    def show[A](source: DialogSource[A]): A = showDialog(source)
  }
}
