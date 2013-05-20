/*
 *  DocumentElementsFrameImpl.scala
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

package de.sciss
package mellite
package gui
package impl

import scala.swing.{ComboBox, Swing, TextField, Dialog, Component, FlowPanel, Action, Button, BorderPanel}
import lucre.stm
import synth.proc.{Grapheme, Artifact, ProcGroup, Sys}
import Swing._
import de.sciss.synth.expr._
import desktop.{FileDialog, DialogSource, Window, Menu}
import scalaswingcontrib.PopupMenu
import desktop.impl.WindowImpl
import synth.io.AudioFile
import scala.util.control.NonFatal
import javax.swing.{JSpinner, SpinnerNumberModel}

object DocumentElementsFrameImpl {
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

    def dispose()(implicit tx: S#Tx) {
      disposeData()
      guiFromTx(comp.dispose())
    }

    private def disposeData()(implicit tx: S#Tx) {
      folderView.dispose()
    }

    def frameClosing() {
      cursor.step { implicit tx =>
        disposeData()
      }
    }

    private def targetFolder(implicit tx: S#Tx): Folder[S] = {
      val sel = folderView.selection
      if (sel.isEmpty) document.elements else sel.head match {
        case (_, _parent: ElementView.Folder[S])        => _parent.folder
        case (_ :+ (_parent: ElementView.Folder[S]), _) => _parent.folder
        case _                                          => document.elements
      }
    }

    private def addElement(elem: Element[S])(implicit tx: S#Tx) {
      val parent = targetFolder
      parent.addLast(elem)
    }

    private def actionAddFolder() {
      val res = Dialog.showInput[String](folderView.component, "Enter initial folder name:", "New Folder",
        Dialog.Message.Question, initial = "Folder")
      res.foreach { name =>
        atomic { implicit tx =>
          addElement(Element.Folder(name, Folder[S]))
        }
      }
    }

    private def actionAddProcGroup() {
      val res = Dialog.showInput[String](folderView.component, "Enter initial group name:", "New ProcGroup",
        Dialog.Message.Question, initial = "Timeline")
      res.foreach { name =>
        atomic { implicit tx =>
          addElement(Element.ProcGroup(name, ProcGroup.Modifiable[S]))
        }
      }
    }

    private def actionAddArtifactLocation() {
      val query = ActionArtifactLocation.queryNew(window = Some(comp))
      query.foreach { case (directory, name) =>
        atomic { implicit tx =>
          ActionArtifactLocation.create(directory, name, targetFolder)
        }
      }
    }

    private def actionAddAudioFile() {
      type Loc      = Element    .ArtifactLocation[S]
      type LocView  = ElementView.ArtifactLocation[S]

      val locViews = folderView.selection.collect {
        case (_, view: LocView) => view
      }

      val dlg = FileDialog.open(init = locViews.headOption.map(_.directory), title = "Add Audio File")
      dlg.setFilter(AudioFile.identify(_).isDefined)
      dlg.show(None).foreach { f =>
        val spec      = AudioFile.readSpec(f)
        val name0     = f.getName
        val i         = name0.lastIndexOf('.')
        val name      = if (i < 0) name0 else name0.substring(0, i)

        val locsOk = locViews.flatMap { view =>
          try {
            Artifact.relativize(view.directory, f)
            Some(view)
          } catch {
            case NonFatal(_) => None
          }
        } .headOption

        val locSourceOpt: Option[stm.Source[S#Tx, Loc]] = locsOk match {
          case Some(loc)  => Some(loc.element)
          case _          =>
            val parent = folderView.selection.collect {
              case (_, f: ElementView.Folder[S]) => f.element
            } .headOption
            ActionArtifactLocation.query(document, file = f, folder = parent, window = Some(comp))
        }

        locSourceOpt.foreach { locSource =>
          atomic { implicit tx =>
            val loc = locSource()
            loc.entity.modifiableOption.foreach { locM =>
              val offset    = Longs  .newVar[S](Longs  .newConst(0L))
              val gain      = Doubles.newVar[S](Doubles.newConst(1.0))
              val artifact  = locM.add(f)
              val audio     = Grapheme.Elem.Audio(artifact, spec, offset, gain)
              addElement(Element.AudioGrapheme(name, audio))
            }
          }
        }
      }
    }

    private def actionAddInt() {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValueJ  = new JSpinner(new SpinnerNumberModel(0, Int.MinValue, Int.MaxValue, 1))
      val ggValue   = Component.wrap(ggValueJ)
      actionAddPrimitive(tpe = "Integer", ggValue = ggValue, prepare = ggValueJ.getValue match {
        case n: java.lang.Number => Some(n.intValue())
        case _  => None
      }) { implicit tx =>
        (name, value) => Element.Int(name, Ints.newVar(value))
      }
    }

    private def actionAddDouble() {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValueJ  = new JSpinner(new SpinnerNumberModel(0.0, Double.NegativeInfinity, Double.PositiveInfinity, 1.0))
      val ggValue   = Component.wrap(ggValueJ)
      actionAddPrimitive(tpe = "Double", ggValue = ggValue, prepare = ggValueJ.getValue match {
        case n: java.lang.Number => Some(n.doubleValue())
        case _  => None
      }) { implicit tx =>
        (name, value) => Element.Double(name, Doubles.newVar(value))
      }
    }

    private def actionAddString() {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      actionAddPrimitive(tpe = "String", ggValue = ggValue, prepare = Some(ggValue.text)){ implicit tx =>
        (name, value) => Element.String(name, Strings.newVar(value))
      }
    }

    private def actionAddCode() {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValue   = new ComboBox(Seq("File Transform"))
      actionAddPrimitive(tpe = "Code", ggValue = ggValue, prepare = ggValue.selection.index match {
        case 0 => Some(Code.FileTransform(
          """
            |val ain   = AudioFile.openRead(in)
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
          """.stripMargin))
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

    def guiInit() {
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

    title           = s"${view.document.folder.nameWithoutExtension} : Elements"
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
