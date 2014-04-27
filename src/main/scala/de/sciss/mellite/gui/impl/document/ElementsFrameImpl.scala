/*
 *  ElementsFrameImpl.scala
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
package document

import scala.swing.{ComboBox, TextField, Dialog, Component, FlowPanel, Action, Button, BorderPanel}
import de.sciss.lucre.stm
import de.sciss.synth.proc.{FolderElem, Elem, IntElem, DoubleElem, Obj, ProcKeys, Folder, ExprImplicits, ProcGroup, StringElem, ProcGroupElem}
import de.sciss.desktop.{FileDialog, DialogSource, Window, Menu}
import de.sciss.synth.io.AudioFile
import javax.swing.SpinnerNumberModel
import de.sciss.file._
import de.sciss.swingplus.{PopupMenu, Spinner}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.{String => StringEx, Double => DoubleEx, Int => IntEx, Expr}
import scala.util.Try
import de.sciss.lucre.swing._
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.synth.proc
import proc.Implicits._

object ElementsFrameImpl {
  def apply[S <: Sys[S], S1 <: Sys[S1]](doc: Document[S], nameOpt: Option[Expr[S1, String]])(implicit tx: S#Tx,
                                        cursor: stm.Cursor[S], bridge: S#Tx => S1#Tx): DocumentElementsFrame[S] = {
    // implicit val csr  = doc.cursor
    val folderView      = FolderView(doc.folder, doc.root())
    val name0           = nameOpt.map(_.value(bridge(tx)))
    val view            = new Impl[S, S1](doc, folderView) {
      protected val nameObserver = nameOpt.map { name =>
        name.changed.react { implicit tx => upd =>
          deferTx(nameUpdate(Some(upd.now)))
        } (bridge(tx))
      }

      deferTx {
        guiInit()
        nameUpdate(name0)
        component.front()
      }
    }

    view
  }

  private abstract class Impl[S <: Sys[S], S1 <: Sys[S1]](val document: Document[S], folderView: FolderView[S])
                                       (implicit val cursor: stm.Cursor[S], bridge: S#Tx => S1#Tx)
    extends DocumentElementsFrame[S] with ComponentHolder[Frame[S]] with CursorHolder[S] {

    // protected implicit def cursor: Cursor[S] = document.cursor

    protected def nameObserver: Option[stm.Disposable[S1#Tx]]

    def dispose()(implicit tx: S#Tx): Unit = {
      disposeData()
      deferTx(component.dispose())
    }

    final protected def nameUpdate(name: Option[String]): Unit = {
      requireEDT()
      component.title = mkTitle(name)
    }

    private def mkTitle(sOpt: Option[String]) = s"${document.folder.base}${sOpt.fold("")(s => s"/$s")} : Elements"

    private def disposeData()(implicit tx: S#Tx): Unit = {
      folderView  .dispose()
      nameObserver.foreach(_.dispose()(bridge(tx)))
    }

    final def frameClosing(): Unit =
      cursor.step { implicit tx =>
        disposeData()
      }

    private def targetFolder(implicit tx: S#Tx): Folder[S] = {
      val sel = folderView.selection
      if (sel.isEmpty) document.root() else sel.head match {
        case (_,    _parent: ObjView.Folder[S])     => _parent.folder()
        case (_ :+ (_parent: ObjView.Folder[S]), _) => _parent.folder()
        case _                                      => document.root()
      }
    }

    private def addObject(obj: Obj[S])(implicit tx: S#Tx): Unit = {
      val parent = targetFolder
      parent.addLast(obj)
    }

    private def actionAddFolder(): Unit = {
      val res = Dialog.showInput[String](folderView.component, "Enter initial folder name:", "New Folder",
        Dialog.Message.Question, initial = "Folder")
      res.foreach { name =>
        atomic { implicit tx =>
          val elem  = FolderElem(Folder[S])
          val obj   = Obj(elem)
          val imp   = ExprImplicits[S]
          import imp._
          obj.attr.name = name
          addObject(obj)
        }
      }
    }

    private def actionAddProcGroup(): Unit = {
      val res = Dialog.showInput[String](folderView.component, "Enter initial group name:", "New ProcGroup",
        Dialog.Message.Question, initial = "Timeline")
      res.foreach { name =>
        atomic { implicit tx =>
          val peer  = ProcGroup.Modifiable[S]
          val elem  = ProcGroupElem(peer)
          val obj   = Obj(elem)
          obj.attr.name = name
          addObject(obj)
        }
      }
    }

    private def actionAddArtifactLocation(): Unit = {
      val query = ActionArtifactLocation.queryNew(window = Some(component))
      query.foreach { case (directory, _name) =>
        atomic { implicit tx =>
          ActionArtifactLocation.create(directory, _name, targetFolder)
        }
      }
    }

    private def actionAddAudioFile(): Unit = {
      val locViews  = folderView.locations
      val dlg       = FileDialog.open(init = locViews.headOption.map(_.directory), title = "Add Audio File")
      dlg.setFilter(f => Try(AudioFile.identify(f).isDefined).getOrElse(false))
      dlg.show(None).foreach { f =>
        val spec          = AudioFile.readSpec(f)
        val locSourceOpt  = folderView.findLocation(f)
        locSourceOpt.foreach { locSource =>
          atomic { implicit tx =>
            val loc = locSource()
            loc.elem.peer.modifiableOption.foreach { locM =>
              ObjectActions.addAudioFile(targetFolder, -1, locM, f, spec)
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
          value => IntElem(IntEx.newVar(value))
      }
    }

    private def actionAddDouble(): Unit = {
      val expr      = ExprImplicits[S]
      import expr._
      val model     = new SpinnerNumberModel(0.0, Double.NegativeInfinity, Double.PositiveInfinity, 1.0)
      val ggValue   = new Spinner(model)
      actionAddPrimitive(tpe = "Double", ggValue = ggValue, prepare = Some(model.getNumber.doubleValue)) {
        implicit tx =>
          value => DoubleElem(DoubleEx.newVar(value))
      }
    }

    private def actionAddString(): Unit = {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      actionAddPrimitive(tpe = "String", ggValue = ggValue, prepare = Some(ggValue.text)){ implicit tx =>
        value => StringElem(StringEx.newVar(value))
      }
    }

    private def actionAddCode(): Unit = {
      val ggValue = new ComboBox(Seq(Code.FileTransform.name, Code.SynthGraph.name))
      actionAddPrimitive(tpe = "Code", ggValue = ggValue, prepare = ggValue.selection.index match {
        case 0 => Some(Code.FileTransform(
          """|val aIn   = AudioFile.openRead(in)
             |val aOut  = AudioFile.openWrite(out, aIn.spec)
             |val bufSz = 8192
             |val buf   = aIn.buffer(bufSz)
             |var rem   = aIn.numFrames
             |while (rem > 0) {
             |  val chunk = math.min(bufSz, rem).toInt
             |  aIn .read (buf, 0, chunk)
             |  // ...
             |  aOut.write(buf, 0, chunk)
             |  rem -= chunk
             |  // checkAbort()
             |}
             |aOut.close()
             |aIn .close()
             |""".stripMargin))

        case 1 => Some(Code.SynthGraph(
          """|val in   = scan.In("in")
             |val sig  = in
             |scan.Out("out", sig)
             |""".stripMargin
        ))

        case _  => None
      }) { implicit tx =>
        value =>
          val peer  = Codes.newVar(Codes.newConst(value))
          Code.Elem(peer)
      }
    }

    private def actionAddPrimitive[A](tpe: String, ggValue: Component, prepare: => Option[A])
                                     (create: S#Tx => A => Elem[S]): Unit = {
      val nameOpt = GUI.keyValueDialog(value = ggValue, title = s"New $tpe", defaultName = tpe,
        window = Some(component))
      nameOpt.foreach { name =>
        prepare.foreach { value =>
          atomic { implicit tx =>
            val elem  = create(tx)(value)
            val obj   = Obj(elem)
            obj.attr.name = name
            addObject(obj)
          }
        }
      }
    }

    final protected def guiInit(): Unit = {
      lazy val addPopup: PopupMenu = {
        import Menu._
        val pop = Popup()
          .add(Item("folder",        Action("Folder"       )(actionAddFolder          ())))
          .add(Item("proc-group",    Action("ProcGroup"    )(actionAddProcGroup       ())))
          .add(Item("artifact-store",Action("ArtifactStore")(actionAddArtifactLocation())))
          .add(Item("audio-file",    Action("Audio File"   )(actionAddAudioFile       ())))
          .add(Item("string",        Action("String"       )(actionAddString          ())))
          .add(Item("int",           Action("Int"          )(actionAddInt             ())))
          .add(Item("double",        Action("Double"       )(actionAddDouble          ())))
          .add(Item("code",          Action("Code"         )(actionAddCode            ())))
        val res = pop.create(component)
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
            case (parent: ObjView.FolderLike[S], child) =>
              parent.folder().remove(child.obj())
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
            case view: ObjView.ProcGroup[S] =>
              // val e   = view.element()
              // import document.inMemory
              TimelineFrame(document, view.obj())

            case view: ObjView.AudioGrapheme[S] =>
              AudioFileFrame(document, view.obj())

            case view: ObjView.Recursion[S] =>
              RecursionFrame(document, view.obj())

            case view: ObjView.Code[S] =>
              CodeFrame(document, view.obj())

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

      component = new Frame(this, folderPanel)

      folderView.addListener {
        case FolderView.SelectionChanged(_, sel) =>
          ggAdd   .enabled  = sel.size < 2
          ggDelete.enabled  = sel.nonEmpty
          ggView  .enabled  = sel.nonEmpty
      }
    }
  }

  private final class Frame[S <: Sys[S]](view: Impl[S, _], _contents: Component) extends WindowImpl {
    file            = Some(view.document.folder)
    closeOperation  = Window.CloseDispose
    reactions += {
      case Window.Closing(_) => view.frameClosing()
    }

    contents        = _contents

    pack()
    // centerOnScreen()
    GUI.placeWindow(this, 0.5f, 0f, 24)

    def show[A](source: DialogSource[A]): A = showDialog(source)
  }
}