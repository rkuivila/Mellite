/*
 *  ObjViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2016 Hanns Holger Rutz. All rights reserved.
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

import java.awt.{Color => AWTColor}
import java.awt.geom.Path2D
import javax.swing.undo.UndoableEdit
import javax.swing.{Icon, SpinnerNumberModel, UIManager}

import de.sciss.audiowidgets.AxisFormat
import de.sciss.desktop
import de.sciss.desktop.OptionPane
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.artifact.{Artifact => _Artifact}
import de.sciss.lucre.event.impl.ObservableImpl
import de.sciss.lucre.expr.{BooleanObj, DoubleObj, LongObj, StringObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.{Disposable, Obj}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.{View, Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.edit.EditFolderInsertObj
import de.sciss.mellite.gui.impl.component.PaintIcon
import de.sciss.mellite.gui.impl.document.NuagesFolderFrameImpl
import de.sciss.model.impl.ModelImpl
import de.sciss.swingplus.{ColorChooser, GroupPanel, Spinner}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{Confluent, ObjKeys, TimeRef}

import scala.swing.Swing.EmptyIcon
import scala.swing.{Action, Alignment, BorderPanel, Button, CheckBox, Component, Dialog, FlowPanel, GridPanel, Label, Swing, TextField}
import scala.util.Try

object ObjViewImpl {
  import java.lang.{String => _String}

  import de.sciss.mellite.{Color => _Color}
  import de.sciss.nuages.{Nuages => _Nuages}
  import de.sciss.synth.proc.{Ensemble => _Ensemble, FadeSpec => _FadeSpec, Folder => _Folder, Timeline => _Timeline, Grapheme => _Grapheme}

  import scala.{Boolean => _Boolean, Double => _Double, Long => _Long}

  def nameOption[S <: stm.Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Option[_String] =
    obj.attr.$[StringObj](ObjKeys.attrName).map(_.value)

  // -------- String --------

  object String extends ListObjView.Factory {
    type E[~ <: stm.Sys[~]] = StringObj[~]
    val icon      = raphaelIcon(raphael.Shapes.Font)
    val prefix    = "String"
    def humanName = prefix
    def tpe = StringObj

    def category = ObjView.categPrimitives

    def mkListView[S <: Sys[S]](obj: StringObj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj
      val value       = ex.value
      val isEditable  = ex match {
        case StringObj.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new String.Impl[S](tx.newHandle(obj), value, isEditable = isEditable, isViewable = isViewable).init(obj)
    }

    type Config[S <: stm.Sys[S]] = PrimitiveConfig[_String]

    def hasMakeDialog = true

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.text))
    }

    def makeObj[S <: Sys[S]](config: (_String, _String))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = StringObj.newVar(StringObj.newConst[S](value))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, StringObj[S]],
                                 var value: _String,
                                 override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _String, StringObj]
      with ListObjViewImpl.StringRenderer {

      type E[~ <: stm.Sys[~]] = StringObj[~]

      def factory = String

      val exprType = StringObj

      def convertEditValue(v: Any): Option[_String] = Some(v.toString)

      def expr(implicit tx: S#Tx) = objH()

      def testValue(v: Any): Option[_String] = v match {
        case s: _String => Some(s)
        case _ => None
      }
    }
  }

  // -------- Long --------

  object Long extends ListObjView.Factory {
    type E[S <: stm.Sys[S]] = LongObj[S]
    val icon      = raphaelIcon(Shapes.IntegerNumbers)  // XXX TODO
    val prefix    = "Long"
    def humanName = prefix
    def tpe    = LongObj
    def hasMakeDialog = true

    def category = ObjView.categPrimitives

    def mkListView[S <: Sys[S]](obj: LongObj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj
      val value       = ex.value
      val isEditable  = ex match {
        case LongObj.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Long.Impl[S](tx.newHandle(obj), value, isEditable = isEditable, isViewable = isViewable).init(obj)
    }

    type Config[S <: stm.Sys[S]] = PrimitiveConfig[_Long]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0L, _Long.MinValue, _Long.MaxValue, 1L)
      val ggValue   = new Spinner(model)
      primitiveConfig[S, _Long](window, tpe = prefix, ggValue = ggValue, prepare = Some(model.getNumber.longValue()))
    }

    def makeObj[S <: Sys[S]](config: (String, _Long))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = LongObj.newVar(LongObj.newConst[S](value))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, LongObj[S]],
                                  var value: _Long,
                                  override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ListObjView /* .Long */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _Long, LongObj]
      with ListObjViewImpl.StringRenderer {

      type E[~ <: stm.Sys[~]] = LongObj[~]

      def factory = Long

      val exprType = LongObj

      def expr(implicit tx: S#Tx): LongObj[S] = objH()

      def convertEditValue(v: Any): Option[_Long] = v match {
        case num: _Long => Some(num)
        case s: _String => Try(s.toLong).toOption
      }

      def testValue(v: Any): Option[_Long] = v match {
        case i: _Long => Some(i)
        case _        => None
      }
    }
  }

  // -------- Double --------

  object Double extends ListObjView.Factory {
    type E[S <: stm.Sys[S]] = DoubleObj[S]
    val icon      = raphaelIcon(Shapes.RealNumbers)
    val prefix    = "Double"
    def humanName = prefix
    def tpe = DoubleObj
    def hasMakeDialog = true

    def category = ObjView.categPrimitives

    def mkListView[S <: Sys[S]](obj: DoubleObj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj
      val value       = ex.value
      val isEditable  = ex match {
        case DoubleObj.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Double.Impl[S](tx.newHandle(obj), value, isEditable = isEditable, isViewable = isViewable).init(obj)
    }

    type Config[S <: stm.Sys[S]] = PrimitiveConfig[_Double]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0.0, _Double.NegativeInfinity, _Double.PositiveInfinity, 1.0)
      val ggValue   = new Spinner(model)
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = Some(model.getNumber.doubleValue))
    }

    def makeObj[S <: Sys[S]](config: (String, _Double))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = DoubleObj.newVar(DoubleObj.newConst[S](value))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, DoubleObj[S]], var value: _Double,
                                  override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ListObjView /* .Double */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _Double, DoubleObj]
      with ListObjViewImpl.StringRenderer {

      type E[~ <: stm.Sys[~]] = DoubleObj[~]

      def factory = Double

      val exprType = DoubleObj

      def expr(implicit tx: S#Tx): DoubleObj[S] = objH()

      def convertEditValue(v: Any): Option[_Double] = v match {
        case num: _Double => Some(num)
        case s: _String => Try(s.toDouble).toOption
      }

      def testValue(v: Any): Option[_Double] = v match {
        case d: _Double => Some(d)
        case _ => None
      }
    }
  }

  // -------- Boolean --------

  object Boolean extends ListObjView.Factory {
    type E[S <: stm.Sys[S]] = BooleanObj[S]
    val icon      = raphaelIcon(Shapes.BooleanNumbers)
    val prefix    = "Boolean"
    def humanName = prefix
    def tpe   = BooleanObj
    def hasMakeDialog = true

    def category = ObjView.categPrimitives

    def mkListView[S <: Sys[S]](obj: BooleanObj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj
      val value       = ex.value
      val isEditable  = ex match {
        case BooleanObj.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Boolean.Impl[S](tx.newHandle(obj), value, isEditable = isEditable, isViewable = isViewable).init(obj)
    }

    type Config[S <: stm.Sys[S]] = PrimitiveConfig[_Boolean]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue = new CheckBox()
      primitiveConfig[S, _Boolean](window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.selected))
    }

    def makeObj[S <: Sys[S]](config: (String, _Boolean))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = BooleanObj.newVar(BooleanObj.newConst[S](value))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, BooleanObj[S]],
                                  var value: _Boolean,
                                  override val isEditable: _Boolean, val isViewable: Boolean)
      extends ListObjView /* .Boolean */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.BooleanExprLike[S]
      with ListObjViewImpl.SimpleExpr[S, _Boolean, BooleanObj] {

      type E[~ <: stm.Sys[~]] = BooleanObj[~]

      def factory = Boolean

      def expr(implicit tx: S#Tx): BooleanObj[S] = objH()
    }
  }

  // -------- Color --------

  object Color extends ListObjView.Factory {
    type E[~ <: stm.Sys[~]] = _Color.Obj[~]
    val icon      = raphaelIcon(raphael.Shapes.Paint)
    val prefix    = "Color"
    def humanName = prefix
    def tpe = _Color.Obj
    def category  = ObjView.categOrganisation

    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: _Color.Obj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj
      val value       = ex.value
      val isEditable  = ex match {
        case _Color.Obj.Var(_)  => true
        case _            => false
      }
      new Color.Impl[S](tx.newHandle(obj), value, isEditable0 = isEditable).init(obj)
    }

    type Config[S <: stm.Sys[S]] = PrimitiveConfig[_Color]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                   (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val (ggValue, ggChooser) = mkColorEditor()
      primitiveConfig[S, _Color](window, tpe = prefix, ggValue = ggValue, prepare = Some(fromAWT(ggChooser.color)))
    }

    private def mkColorEditor(): (Component, ColorChooser) = {
      val chooser = new ColorChooser()
      val bPredef = _Color.Palette.map { colr =>
        val action = new Action(null /* colr.name */) {
          private val awtColor = toAWT(colr)
          icon = new PaintIcon(awtColor, 32, 32)
          def apply(): Unit = chooser.color = awtColor
        }
        val b = new Button(action)
        // b.horizontalAlignment = Alignment.Left
        b.focusable = false
        b
      }
      val pPredef = new GridPanel(4, 4)
      pPredef.contents ++= bPredef
      val panel = new BorderPanel {
        add(pPredef, BorderPanel.Position.West  )
        add(chooser, BorderPanel.Position.Center)
      }
      (panel, chooser)
    }

    def toAWT(c: _Color): java.awt.Color = new java.awt.Color(c.rgba)
    def fromAWT(c: java.awt.Color): _Color = {
      val rgba = c.getRGB
      _Color.Palette.find(_.rgba == rgba).getOrElse(_Color.User(rgba))
    }

    def makeObj[S <: Sys[S]](config: (String, _Color))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = _Color.Obj.newVar(_Color.Obj.newConst[S](value))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Color.Obj[S]],
                                  var value: _Color, isEditable0: _Boolean)
      extends ListObjView /* .Color */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _Color, _Color.Obj] {

      type E[~ <: stm.Sys[~]] = _Color.Obj[~]

      def isEditable = false    // not until we have proper editing components

      def factory = Color

      val exprType = _Color.Obj

      def expr(implicit tx: S#Tx): _Color.Obj[S] = objH()

      def configureRenderer(label: Label): Component = {
        // renderers are used for "stamping", so we can reuse a single object.
        label.icon = ListIcon
        ListIcon.paint = Color.toAWT(value)
        label
      }

      def convertEditValue(v: Any): Option[_Color] = testValue(v) // XXX TODO -- what was the difference again

      def testValue(v: Any): Option[_Color] = v match {
        case c: _Color  => Some(c)
        case _          => None
      }

      def isViewable = isEditable0

      override def openView(parent: Option[Window[S]])
                           (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
//        val opt = OptionPane.confirmation(message = component, optionType = OptionPane.Options.OkCancel,
//          messageType = OptionPane.Message.Plain)
//        opt.show(parent) === OptionPane.Result.Ok
        val title = AttrCellView.name(obj)
        val w = new WindowImpl[S](title) { self =>
          val view: View[S] = View.wrap {
            val (compColor, chooser) = Color.mkColorEditor()
            chooser.color = Color.toAWT(value)
            val ggCancel = Button("Cancel") {
              closeMe() // self.handleClose()
            }

            def apply(): Unit = {
              val colr = Color.fromAWT(chooser.color)
              val editOpt = cursor.step { implicit tx =>
                objH() match {
                  case _Color.Obj.Var(vr) =>
                    implicit val colorTpe = _Color.Obj
                    Some(EditVar.Expr[S, _Color, _Color.Obj]("Change Color", vr, _Color.Obj.newConst[S](colr)))
                  case _ => None
                }
              }
              editOpt.foreach { edit =>
                parent.foreach { p =>
                  p.view match {
                    case e: View.Editable[S] => e.undoManager.add(edit)
                  }
                }
              }
            }

            val ggOk = Button("Ok") {
              apply()
              closeMe() // self.handleClose()
            }
            val ggApply = Button("Apply") {
              apply()
            }
            val pane = new BorderPanel {
              add(compColor, BorderPanel.Position.Center)
              add(new FlowPanel(ggOk, ggApply, Swing.HStrut(8), ggCancel), BorderPanel.Position.South)
            }
            pane
          }

          def closeMe(): Unit = cursor.step { implicit tx => self.dispose() }

          init()
        }
        Some(w)
      }
    }

    private val ListIcon = new PaintIcon(java.awt.Color.black, 48, 16)
  }

  // -------- Artifact --------

  object Artifact extends ListObjView.Factory {
    type E[~ <: stm.Sys[~]] = _Artifact[~]
    val icon      = raphaelIcon(raphael.Shapes.PagePortrait)
    val prefix    = "Artifact"
    def humanName = "File"
    def tpe = _Artifact
    def hasMakeDialog = false

    def category = ObjView.categResources

    def mkListView[S <: Sys[S]](obj: _Artifact[S])(implicit tx: S#Tx): ListObjView[S] = {
      val peer      = obj
      val value     = peer.value  // peer.child.path
      val editable  = false // XXX TODO -- peer.modifiableOption.isDefined
      new Artifact.Impl[S](tx.newHandle(obj), value, isEditable = editable).init(obj)
    }

    type Config[S <: stm.Sys[S]] = PrimitiveConfig[File]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = None // XXX TODO

    def makeObj[S <: Sys[S]](config: (_String, File))(implicit tx: S#Tx): List[Obj[S]] = ???!

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Artifact[S]],
                                  var file: File, val isEditable: _Boolean)
      extends ListObjView /* .Artifact */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.StringRenderer
      with NonViewable[S] {

      type E[~ <: stm.Sys[~]] = _Artifact[~]

      def factory = Artifact

      def value   = file

      def init(obj: _Artifact[S])(implicit tx: S#Tx): this.type = {
        initAttrs(obj)
        disposables ::= obj.changed.react { implicit tx => upd =>
          deferTx {
            file = upd.now
          }
          fire(ObjView.Repaint(this))
        }
        this
      }

      def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = None // XXX TODO
    }
  }

  // -------- Folder --------

  object Folder extends ListObjView.Factory {
    type E[~ <: stm.Sys[~]] = _Folder[~]
    def icon      = UIManager.getIcon("Tree.openIcon")  // Swing.EmptyIcon
    val prefix    = "Folder"
    def humanName = prefix
    def tpe       = _Folder
    def category  = ObjView.categOrganisation

    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: _Folder[S])(implicit tx: S#Tx): ListObjView[S] =
      new Folder.Impl[S](tx.newHandle(obj)).initAttrs(obj)

    type Config[S <: stm.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S],window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = "New Folder"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val obj  = _Folder[S]
      obj.name = name
      obj :: Nil
    }

    // XXX TODO: could be viewed as a new folder view with this folder as root
    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Folder[S]])
      extends ListObjView /* .Folder */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with ListObjViewImpl.NonEditable[S] {

      type E[~ <: stm.Sys[~]] = _Folder[~]

      def factory = Folder

      def isViewable = true

      def openView(parent: Option[Window[S]])
                  (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val folderObj = objH()
        val nameView  = AttrCellView.name(folderObj)
        Some(FolderFrame(nameView, folderObj))
      }
    }
  }

  // -------- Timeline --------

  object Timeline extends ListObjView.Factory {
    type E[S <: stm.Sys[S]] = _Timeline[S]
    val icon      = raphaelIcon(raphael.Shapes.Ruler)
    val prefix    = "Timeline"
    def humanName = prefix
    def tpe = _Timeline
    def hasMakeDialog = true

    def category = ObjView.categComposition

    def mkListView[S <: Sys[S]](obj: _Timeline[S])(implicit tx: S#Tx): ListObjView[S] =
      new Timeline.Impl(tx.newHandle(obj)).initAttrs(obj)

    type Config[S <: stm.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val obj = _Timeline[S] // .Modifiable[S]
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Timeline[S]])
      extends ListObjView /* .Timeline */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with ListObjViewImpl.NonEditable[S] {

      type E[~ <: stm.Sys[~]] = _Timeline[~]

      def factory = Timeline

      def isViewable = true

      def openView(parent: Option[Window[S]])
                  (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
//        val message = s"<html><b>Select View Type for $name:</b></html>"
//        val entries = Seq("Timeline View", "Real-time View", "Cancel")
//        val opt = OptionPane(message = message, optionType = OptionPane.Options.YesNoCancel,
//          messageType = OptionPane.Message.Question, entries = entries)
//        opt.title = s"View $name"
//        (opt.show(None).id: @switch) match {
//          case 0 =>
            val frame = TimelineFrame[S](objH())
            Some(frame)
//          case 1 =>
//            val frame = InstantGroupFrame[S](document, obj())
//            Some(frame)
//          case _ => None
//        }
      }
    }
  }

  // -------- Grapheme --------

  object Grapheme extends ListObjView.Factory {
    type E[S <: stm.Sys[S]] = _Grapheme[S]
    val icon          = raphaelIcon(raphael.Shapes.LineChart)
    val prefix        = "Grapheme"
    def humanName     = prefix
    def tpe           = _Grapheme
    def hasMakeDialog = true
    def category      = ObjView.categComposition

    def mkListView[S <: Sys[S]](obj: _Grapheme[S])(implicit tx: S#Tx): ListObjView[S] =
      new Grapheme.Impl(tx.newHandle(obj)).initAttrs(obj)

    type Config[S <: stm.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                   (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val obj = _Grapheme[S] // .Modifiable[S]
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Grapheme[S]])
      extends ListObjView /* .Grapheme */[S]
        with ObjViewImpl.Impl[S]
        with ListObjViewImpl.EmptyRenderer[S]
        with ListObjViewImpl.NonEditable[S] {

      type E[~ <: stm.Sys[~]] = _Grapheme[~]

      def factory = Grapheme

      def isViewable = true

      def openView(parent: Option[Window[S]])
                  (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        None
//        val frame = GraphemeFrame[S](objH())
//        Some(frame)
      }
    }
  }

  // -------- FadeSpec --------

  object FadeSpec extends ListObjView.Factory {
    type E[~ <: stm.Sys[~]] = _FadeSpec.Obj[~]
    // val icon        = raphaelIcon(raphael.Shapes.Up)
    val icon        = raphaelIcon(Shapes.Aperture)
    val prefix      = "FadeSpec"
    val humanName   = "Fade"
    def tpe         = _FadeSpec.Obj
    def category    = ObjView.categComposition

    def hasMakeDialog   = false

    def mkListView[S <: Sys[S]](obj: _FadeSpec.Obj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val value   = obj.value
      new FadeSpec.Impl[S](tx.newHandle(obj), value).init(obj)
    }

    type Config[S <: stm.Sys[S]] = Unit

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      None
//      val ggShape = new ComboBox()
//      Curve.cubed
//      val ggValue = new ComboBox(Seq(_Code.FileTransform.name, _Code.SynthGraph.name))
//      actionAddPrimitive(folderH, window, tpe = prefix, ggValue = ggValue, prepare = ...
//      ) { implicit tx =>
//        value =>
//          val peer = _FadeSpec.Expr(numFrames, shape, floor)
//          _FadeSpec.Obj(peer)
//      }
    }

    def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = Nil

    private val timeFmt = AxisFormat.Time(hours = false, millis = true)

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _FadeSpec.Obj[S]], var value: _FadeSpec)
      extends ListObjView /* .FadeSpec */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.NonEditable[S]
      with NonViewable[S] {

      type E[~ <: stm.Sys[~]] = _FadeSpec.Obj[~]

      def factory = FadeSpec

      def init(obj: _FadeSpec.Obj[S])(implicit tx: S#Tx): this.type = {
        initAttrs(obj)
        disposables ::= obj.changed.react { implicit tx => upd =>
          deferTx {
            value = upd.now
          }
          fire(ObjView.Repaint(this))
        }
        this
      }

      def configureRenderer(label: Label): Component = {
        val sr = TimeRef.SampleRate // 44100.0
        val dur = timeFmt.format(value.numFrames.toDouble / sr)
        label.text = s"$dur, ${value.curve}"
        label
      }
    }
  }

  // -------- Ensemble --------

  object Ensemble extends ListObjView.Factory {
    type E[~ <: stm.Sys[~]] = _Ensemble[~]
    val icon        = raphaelIcon(raphael.Shapes.Cube2)
    val prefix      = "Ensemble"
    def humanName   = prefix
    def tpe = _Ensemble
    def category    = ObjView.categComposition

    def hasMakeDialog   = true

    def mkListView[S <: Sys[S]](obj: _Ensemble[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ens     = obj
      val playingEx = ens.playing
      val playing = playingEx.value
      val isEditable  = playingEx match {
        case BooleanObj.Var(_)  => true
        case _            => false
      }
      new Ensemble.Impl[S](tx.newHandle(obj), playing = playing, isEditable = isEditable).init(obj)
    }

    final case class Config[S <: stm.Sys[S]](name: String, offset: Long, playing: Boolean)

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggName    = new TextField(10)
      ggName.text   = prefix
      val offModel  = new SpinnerNumberModel(0.0, 0.0, 1.0e6 /* _Double.MaxValue */, 0.1)
      val ggOff     = new Spinner(offModel)
      // doesn't work
      //      // using Double.MaxValue causes panic in spinner's preferred-size
      //      ggOff.preferredSize = new Dimension(ggName.preferredSize.width, ggOff.preferredSize.height)
      //      ggOff.maximumSize   = ggOff.preferredSize
      val ggPlay    = new CheckBox

      val lbName  = new Label(       "Name:", EmptyIcon, Alignment.Right)
      val lbOff   = new Label( "Offset [s]:", EmptyIcon, Alignment.Right)
      val lbPlay  = new Label(    "Playing:", EmptyIcon, Alignment.Right)

      val box = new GroupPanel {
        horizontal  = Seq(Par(Trailing)(lbName, lbOff, lbPlay), Par(ggName , ggOff, ggPlay))
        vertical    = Seq(Par(Baseline)(lbName, ggName),
          Par(Baseline)(lbOff , ggOff ),
          Par(Baseline)(lbPlay, ggPlay))
      }

      val pane = desktop.OptionPane.confirmation(box, optionType = Dialog.Options.OkCancel,
        messageType = Dialog.Message.Question, focus = Some(ggName))
      pane.title  = s"New $prefix"
      val res = pane.show(window)

      if (res != Dialog.Result.Ok) None else {
        val name      = ggName.text
        val seconds   = offModel.getNumber.doubleValue()
        val offset    = (seconds * TimeRef.SampleRate + 0.5).toLong
        val playing   = ggPlay.selected
        Some(Config(name = name, offset = offset, playing = playing))
      }
    }

    def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
      val folder    = _Folder[S] // XXX TODO - can we ask the user to pick one?
      val offset    = LongObj   .newVar(LongObj   .newConst[S](config.offset ))
      val playing   = BooleanObj.newVar(BooleanObj.newConst[S](config.playing))
      val obj      = _Ensemble[S](folder, offset, playing)
      obj.name = config.name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Ensemble[S]],
                                  var playing: _Boolean, val isEditable: Boolean)
      extends ListObjView /* .Ensemble */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.BooleanExprLike[S] {

      type E[~ <: stm.Sys[~]] = _Ensemble[~]

      def factory = Ensemble

      def isViewable = true

      protected def exprValue: _Boolean = playing
      protected def exprValue_=(x: _Boolean): Unit = playing = x
      protected def expr(implicit tx: S#Tx): BooleanObj[S] = objH().playing

      def value: Any = ()

      def init(obj: _Ensemble[S])(implicit tx: S#Tx): this.type = {
        initAttrs(obj)
        disposables ::= obj.changed.react { implicit tx => upd =>
          upd.changes.foreach {
            case _Ensemble.Playing(ch) =>
              deferTx {
                playing = ch.now
              }
              fire(ObjView.Repaint(this))

            case _ =>
          }
        }
        this
      }

      override def openView(parent: Option[Window[S]])
                           (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val ens   = objH()
        val w     = EnsembleFrame(ens)
        Some(w)
      }
    }
  }

  // -------- Nuages --------

  object Nuages extends ListObjView.Factory {
    type E[S <: stm.Sys[S]] = _Nuages[S]
    val icon        = raphaelIcon(raphael.Shapes.CloudWhite)
    val prefix      = "Nuages"
    val humanName   = "Wolkenpumpe"
    def tpe = _Nuages
    def hasMakeDialog   = true

    def category = ObjView.categComposition

    def mkListView[S <: Sys[S]](obj: _Nuages[S])(implicit tx: S#Tx): ListObjView[S] =
      new Nuages.Impl[S](tx.newHandle(obj)).initAttrs(obj)

    type Config[S <: stm.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val tl  = _Timeline[S]
      val obj = _Nuages[S](_Nuages.Surface.Timeline(tl))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val objH: stm.Source[S#Tx, _Nuages[S]])
      extends ListObjView /* .Nuages */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.NonEditable[S]
      with ListObjViewImpl.EmptyRenderer[S] {

      type E[~ <: stm.Sys[~]] = _Nuages[~]

      def factory = Nuages

      def isViewable = true

      def openView(parent: Option[Window[S]])
                  (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val frame = NuagesFolderFrameImpl(objH())
        Some(frame)
      }
    }
  }

  // -----------------------------

  def addObject[S <: Sys[S]](name: String, parent: _Folder[S], obj: Obj[S])
                            (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    // val parent = targetFolder
    // parent.addLast(obj)
    val idx = parent.size
    implicit val folderSer = _Folder.serializer[S]
    EditFolderInsertObj[S](name, parent, idx, obj)
  }

  type PrimitiveConfig[A] = (String, A)

  def primitiveConfig[S <: Sys[S], A](window: Option[desktop.Window], tpe: String, ggValue: Component,
                                      prepare: => Option[A]): Option[PrimitiveConfig[A]] = {
    val nameOpt = GUI.keyValueDialog(value = ggValue, title = s"New $tpe", defaultName = tpe, window = window)
    for {
      name  <- nameOpt
      value <- prepare
    } yield {
      (name, value)
    }
  }

  private[this] val colrIconDark = new AWTColor(200, 200, 200)

  def raphaelIcon(shape: Path2D => Unit): Icon = {
    val fill = if (Mellite.isDarkSkin) colrIconDark else AWTColor.black
    raphael.Icon(extent = 16, fill = fill)(shape)
  }

  trait Impl[S <: stm.Sys[S]] extends ObjView[S] /* with ModelImpl[ObjView.Update[S]] */
    with ObservableImpl[S, ObjView.Update[S]] {

    override def toString = s"ElementView.${factory.prefix}(name = $name)"

    def objH: stm.Source[S#Tx, Obj[S]]

    def obj(implicit tx: S#Tx): Obj[S] = objH()

    /** Forwards to factory. */
    def humanName: String = factory.humanName

    /** Forwards to factory. */
    def icon: Icon = factory.icon

    var nameOption : Option[String] = None
    var colorOption: Option[Color ] = None

    protected var disposables = List.empty[Disposable[S#Tx]]

    def dispose()(implicit tx: S#Tx): Unit = disposables.foreach(_.dispose())

    /** Sets name and color. */
    def initAttrs(obj: Obj[S])(implicit tx: S#Tx): this.type = {
      val attr      = obj.attr

      implicit val stringTpe = StringObj
      val nameView  = AttrCellView[S, String, StringObj](attr, ObjKeys.attrName)
      disposables ::= nameView.react { implicit tx => opt => deferTx {
        nameOption = opt
      }}
      nameOption   = nameView()

      implicit val colorTpe = _Color.Obj
      val colorView = AttrCellView[S, _Color, _Color.Obj](attr, ObjView.attrColor)
      disposables ::= colorView.react { implicit tx => opt => deferTx {
        colorOption = opt
      }}
      colorOption  = colorView()
      this
    }
  }

  /** A trait that when mixed in provides `isViewable` and `openView` as non-op methods. */
  trait NonViewable[S <: stm.Sys[S]] {
    def isViewable: _Boolean = false

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = None
  }
}
