/*
 *  ObjViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
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

import java.awt.geom.Path2D
import javax.swing.undo.UndoableEdit
import javax.swing.{Icon, SpinnerNumberModel, UIManager}

import de.sciss.audiowidgets.AxisFormat
import de.sciss.desktop.OptionPane
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{Boolean => BooleanEx, Double => DoubleEx, Expr, Int => IntEx, Long => LongEx, String => StringEx}
import de.sciss.lucre.swing.{Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.edit.EditFolderInsertObj
import de.sciss.mellite.gui.impl.document.NuagesFolderFrameImpl
import de.sciss.model.Change
import de.sciss.swingplus.{GroupPanel, Spinner}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.impl.{ElemImpl, FolderElemImpl}
import de.sciss.synth.proc.{ArtifactElem, BooleanElem, Confluent, DoubleElem, ExprImplicits, FolderElem, LongElem, Obj, ObjKeys, StringElem}
import de.sciss.{desktop, lucre}

import scala.swing.Swing.EmptyIcon
import scala.swing.{Alignment, CheckBox, Component, Dialog, Label, TextField}
import scala.util.Try

object ObjViewImpl {
  import java.lang.{String => _String}

  import de.sciss.lucre.artifact.{Artifact => _Artifact, ArtifactLocation => _ArtifactLocation}
  import de.sciss.mellite.{Recursion => _Recursion}
  import de.sciss.nuages.{Nuages => _Nuages}
  import de.sciss.synth.proc.{Action => _Action, Code => _Code, Ensemble => _Ensemble, FadeSpec => _FadeSpec, Folder => _Folder, Proc => _Proc, Timeline => _Timeline}

  import scala.{Boolean => _Boolean, Double => _Double, Int => _Int, Long => _Long}
  
  def nameOption[S <: evt.Sys[S]](obj: Obj[S])(implicit tx: S#Tx): Option[_String] =
    obj.attr[StringElem](ObjKeys.attrName).map(_.value)
  
  // -------- String --------

  object String extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = StringElem[S]
    val icon      = raphaelIcon(raphael.Shapes.Font)
    val prefix    = "String"
    def typeID    = ElemImpl.String.typeID
    
    def mkListView[S <: Sys[S]](obj: Obj.T[S, StringElem])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new String.Impl(tx.newHandle(obj), nameOption(obj), value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: evt.Sys[S]] = PrimitiveConfig[_String]

    def hasMakeDialog = true

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.text))
    }

    def makeObj[S <: Sys[S]](config: (_String, _String))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(StringElem(StringEx.newVar(StringEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, StringElem]],
                                 var nameOption: Option[_String], var value: _String,
                                 override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ListObjView[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _String]
      with ListObjViewImpl.StringRenderer {

      type E[~ <: evt.Sys[~]] = StringElem[~]

      def prefix  = String.prefix
      def icon    = String.icon
      def typeID  = String.typeID

      def exprType = lucre.expr.String

      def convertEditValue(v: Any): Option[_String] = Some(v.toString)

      def expr(implicit tx: S#Tx) = obj().elem.peer

      def testValue(v: Any): Option[_String] = v match {
        case s: _String => Some(s)
        case _ => None
      }
    }
  }

  // -------- Long --------

  object Long extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = LongElem[S]
    val icon      = raphaelIcon(Shapes.IntegerNumbers)  // XXX TODO
    val prefix    = "Long"
    def typeID    = ElemImpl.Long.typeID
    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: Obj.T[S, LongElem])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Long.Impl(tx.newHandle(obj), nameOption(obj), value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: evt.Sys[S]] = PrimitiveConfig[_Long]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0L, _Long.MinValue, _Long.MaxValue, 1L)
      val ggValue   = new Spinner(model)
      primitiveConfig[S, _Long](window, tpe = prefix, ggValue = ggValue, prepare = Some(model.getNumber.longValue()))
    }

    def makeObj[S <: Sys[S]](config: (String, _Long))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(LongElem(LongEx.newVar(LongEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, LongElem]],
                                  var nameOption: Option[_String], var value: _Long,
                                  override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ListObjView /* .Long */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _Long]
      with ListObjViewImpl.StringRenderer {

      type E[~ <: evt.Sys[~]] = LongElem[~]

      def prefix  = Long.prefix
      def icon    = Long.icon
      def typeID  = Long.typeID

      def exprType = LongEx

      def expr(implicit tx: S#Tx): Expr[S, _Long] = obj().elem.peer

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
    type E[S <: evt.Sys[S]] = DoubleElem[S]
    val icon      = raphaelIcon(Shapes.RealNumbers)
    val prefix    = "Double"
    def typeID    = ElemImpl.Double.typeID
    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: Obj.T[S, DoubleElem])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Double.Impl(tx.newHandle(obj), nameOption(obj), value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: evt.Sys[S]] = PrimitiveConfig[_Double]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0.0, _Double.NegativeInfinity, _Double.PositiveInfinity, 1.0)
      val ggValue   = new Spinner(model)
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = Some(model.getNumber.doubleValue))
    }

    def makeObj[S <: Sys[S]](config: (String, _Double))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(DoubleElem(DoubleEx.newVar(DoubleEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]],
                                                   var nameOption: Option[_String], var value: _Double,
                                                   override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ListObjView /* .Double */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.SimpleExpr[S, _Double]
      with ListObjViewImpl.StringRenderer {

      type E[~ <: evt.Sys[~]] = DoubleElem[~]

      def prefix  = Double.prefix
      def icon    = Double.icon
      def typeID  = Double.typeID

      def exprType = DoubleEx

      def expr(implicit tx: S#Tx): Expr[S, _Double] = obj().elem.peer

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
    type E[S <: evt.Sys[S]] = BooleanElem[S]
    val icon      = raphaelIcon(Shapes.BooleanNumbers)
    val prefix    = "Boolean"
    def typeID    = ElemImpl.Boolean.typeID
    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: Obj.T[S, BooleanElem])(implicit tx: S#Tx): ListObjView[S] = {
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Boolean.Impl(tx.newHandle(obj), nameOption(obj), value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: evt.Sys[S]] = PrimitiveConfig[_Boolean]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue = new CheckBox()
      primitiveConfig[S, _Boolean](window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.selected))
    }

    def makeObj[S <: Sys[S]](config: (String, _Boolean))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(BooleanElem(BooleanEx.newVar(BooleanEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, BooleanElem]],
                                  var nameOption: Option[_String], var value: _Boolean,
                                  override val isEditable: _Boolean, val isViewable: Boolean)
      extends ListObjView /* .Boolean */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.BooleanExprLike[S]
      with ListObjViewImpl.SimpleExpr[S, _Boolean] {

      type E[~ <: evt.Sys[~]] = BooleanElem[~]

      def prefix  = Boolean.prefix
      def icon    = Boolean.icon
      def typeID  = Boolean.typeID

      def expr(implicit tx: S#Tx): Expr[S, _Boolean] = obj().elem.peer
    }
  }

  // -------- Artifact --------

  object Artifact extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = ArtifactElem[S]
    val icon      = raphaelIcon(raphael.Shapes.PagePortrait)
    val prefix    = "Artifact"
    def typeID    = ElemImpl.Artifact.typeID
    def hasMakeDialog = false

    def mkListView[S <: Sys[S]](obj: ArtifactElem.Obj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val peer      = obj.elem.peer
      val value     = peer.value  // peer.child.path
      val editable  = false // XXX TODO -- peer.modifiableOption.isDefined
      new Artifact.Impl(tx.newHandle(obj), nameOption(obj), value, isEditable = editable)
    }

    type Config[S <: evt.Sys[S]] = PrimitiveConfig[File]

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = None // XXX TODO

    def makeObj[S <: Sys[S]](config: (_String, File))(implicit tx: S#Tx): List[Obj[S]] = ???

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, ArtifactElem.Obj[S]],
                                  var nameOption: Option[_String], var file: File, val isEditable: _Boolean)
      extends ListObjView /* .Artifact */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.StringRenderer
      with NonViewable[S] {

      type E[~ <: evt.Sys[~]] = ArtifactElem[~]

      def icon    = Artifact.icon
      def prefix  = Artifact.prefix
      def typeID  = Artifact.typeID

      def value   = file

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: File) =>
          deferTx { file = now }
          true
        case _ => false
      }

      def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = None // XXX TODO
    }
  }

  // -------- Recursion --------

  object Recursion extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = _Recursion.Elem[S]
    val icon      = raphaelIcon(raphael.Shapes.Quote)
    val prefix    = "Recursion"
    def typeID    = _Recursion.typeID
    def hasMakeDialog = false

    def mkListView[S <: Sys[S]](obj: Obj.T[S, _Recursion.Elem])(implicit tx: S#Tx): ListObjView[S] = {
      val value     = obj.elem.peer.deployed.elem.peer.artifact.value
      new Recursion.Impl(tx.newHandle(obj), nameOption(obj), value)
    }

    type Config[S <: evt.Sys[S]] = Unit

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = None

    def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = Nil

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Recursion.Elem]],
                                  var nameOption: Option[_String], var deployed: File)
      extends ListObjView /* .Recursion */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.NonEditable[S] {

      type E[~ <: evt.Sys[~]] = _Recursion.Elem[~]

      def icon    = Recursion.icon
      def prefix  = Recursion.prefix
      def typeID  = Recursion.typeID

      def value   = deployed

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false  // XXX TODO

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        import de.sciss.mellite.Mellite.compiler
        val frame = RecursionFrame(obj())
        Some(frame)
      }

      def configureRenderer(label: Label): Component = {
        label.text = deployed.name
        label
      }
    }
  }

  // -------- Folder --------

  object Folder extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = FolderElem[S]
    def icon      = UIManager.getIcon("Tree.openIcon")  // Swing.EmptyIcon
    val prefix    = "Folder"
    def typeID    = FolderElemImpl.typeID
    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: Obj.T[S, FolderElem])(implicit tx: S#Tx): ListObjView[S] =
      new Folder.Impl(tx.newHandle(obj), nameOption(obj))

    type Config[S <: evt.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S],window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = "New Folder"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val elem  = FolderElem(_Folder[S])
      val obj   = Obj(elem)
      val imp   = ExprImplicits[S]
      obj.name = name
      obj :: Nil
    }

    // XXX TODO: could be viewed as a new folder view with this folder as root
    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, FolderElem]],
                                  var nameOption: Option[_String])
      extends ListObjView /* .Folder */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with ListObjViewImpl.NonEditable[S] {

      type E[~ <: evt.Sys[~]] = FolderElem[~]

      def prefix  = Folder.prefix
      def icon    = Folder.icon
      def typeID  = Folder.typeID

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val folderObj = obj()
        val nameView  = AttrCellView.name(folderObj)
        Some(FolderFrame(nameView, folderObj.elem.peer))
      }
    }
  }

  // -------- Timeline --------

  object Timeline extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = _Timeline.Elem[S]
    val icon      = raphaelIcon(raphael.Shapes.Ruler)
    val prefix    = "Timeline"
    def typeID    = _Timeline.typeID
    def hasMakeDialog = true

    def mkListView[S <: Sys[S]](obj: Obj.T[S, _Timeline.Elem])(implicit tx: S#Tx): ListObjView[S] =
      new Timeline.Impl(tx.newHandle(obj), nameOption(obj))

    type Config[S <: evt.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val peer = _Timeline[S] // .Modifiable[S]
      val elem = _Timeline.Elem(peer)
      val obj = Obj(elem)
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Timeline.Elem]],
                                  var nameOption: Option[_String])
      extends ListObjView /* .Timeline */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.EmptyRenderer[S]
      with ListObjViewImpl.NonEditable[S] {

      type E[~ <: evt.Sys[~]] = _Timeline.Elem[~]

      def icon    = Timeline.icon
      def prefix  = Timeline.prefix
      def typeID  = Timeline.typeID

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
//        val message = s"<html><b>Select View Type for $name:</b></html>"
//        val entries = Seq("Timeline View", "Real-time View", "Cancel")
//        val opt = OptionPane(message = message, optionType = OptionPane.Options.YesNoCancel,
//          messageType = OptionPane.Message.Question, entries = entries)
//        opt.title = s"View $name"
//        (opt.show(None).id: @switch) match {
//          case 0 =>
            val frame = TimelineFrame[S](obj())
            Some(frame)
//          case 1 =>
//            val frame = InstantGroupFrame[S](document, obj())
//            Some(frame)
//          case _ => None
//        }
      }
    }
  }

  // -------- FadeSpec --------

  object FadeSpec extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = _FadeSpec.Elem[S]
    val icon        = raphaelIcon(raphael.Shapes.Up)
    val prefix      = "FadeSpec"
    def typeID      = ElemImpl.FadeSpec.typeID
    def hasMakeDialog   = false

    def mkListView[S <: Sys[S]](obj: Obj.T[S, _FadeSpec.Elem])(implicit tx: S#Tx): ListObjView[S] = {
      val value   = obj.elem.peer.value
      new FadeSpec.Impl(tx.newHandle(obj), nameOption(obj), value)
    }

    type Config[S <: evt.Sys[S]] = Unit

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
//          _FadeSpec.Elem(peer)
//      }
    }

    def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = Nil

    private val timeFmt = AxisFormat.Time(hours = false, millis = true)

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _FadeSpec.Elem]],
                                  var nameOption: Option[_String], var value: _FadeSpec)
      extends ListObjView /* .FadeSpec */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.NonEditable[S]
      with NonViewable[S] {

      type E[~ <: evt.Sys[~]] = _FadeSpec.Elem[~]

      def icon    = FadeSpec.icon
      def prefix  = FadeSpec.prefix
      def typeID  = FadeSpec.typeID

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, valueNew: _FadeSpec) =>
          deferTx {
            value = valueNew
          }
          true
        case _ => false
      }


      def configureRenderer(label: Label): Component = {
        val sr = _Timeline.SampleRate // 44100.0
        val dur = timeFmt.format(value.numFrames.toDouble / sr)
        label.text = s"$dur, ${value.curve}"
        label
      }
    }
  }

  // -------- Ensemble --------

  object Ensemble extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = _Ensemble.Elem[S]
    val icon        = raphaelIcon(raphael.Shapes.Cube2)
    val prefix      = "Ensemble"
    def typeID      = _Ensemble.typeID
    def hasMakeDialog   = true

    def mkListView[S <: Sys[S]](obj: _Ensemble.Obj[S])(implicit tx: S#Tx): ListObjView[S] = {
      val ens     = obj.elem.peer
      val playingEx = ens.playing
      val playing = playingEx.value
      val isEditable  = playingEx match {
        case Expr.Var(_)  => true
        case _            => false
      }
      new Ensemble.Impl(tx.newHandle(obj), nameOption(obj), playing = playing, isEditable = isEditable)
    }

    final case class Config[S <: evt.Sys[S]](name: String, offset: Long, playing: Boolean)

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
        val offset    = (seconds * _Timeline.SampleRate + 0.5).toLong
        val playing   = ggPlay.selected
        Some(Config(name = name, offset = offset, playing = playing))
      }
    }

    def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
      val folder    = _Folder[S] // XXX TODO - can we ask the user to pick one?
      val offset    = LongEx   .newVar(LongEx   .newConst[S](config.offset ))
      val playing   = BooleanEx.newVar(BooleanEx.newConst[S](config.playing))
      val elem      = _Ensemble.Elem(_Ensemble[S](folder, offset, playing))
      val obj       = Obj(elem)
      obj.name = config.name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, _Ensemble.Obj[S]],
                                  var nameOption: Option[_String], var playing: _Boolean, val isEditable: Boolean)
      extends ListObjView /* .Ensemble */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.BooleanExprLike[S] {

      type E[~ <: evt.Sys[~]] = _Ensemble.Elem[~]

      def icon    = Ensemble.icon
      def prefix  = Ensemble.prefix
      def typeID  = Ensemble.typeID

      def isViewable = true

      protected def exprValue: _Boolean = playing
      protected def exprValue_=(x: _Boolean): Unit = playing = x
      protected def expr(implicit tx: S#Tx): Expr[S, _Boolean] = obj().elem.peer.playing

      def value: Any = ()

      override def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case _Ensemble.Update(_, changes) =>
          (false /: changes) {
            case (_, _Ensemble.Playing(Change(_, v))) => playing = v; true
            case (res, _) => res
          }
      }

      override def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val ens   = obj()
        val w     = EnsembleFrame(ens)
        Some(w)
      }
    }
  }

  // -------- Nuages --------

  object Nuages extends ListObjView.Factory {
    type E[S <: evt.Sys[S]] = _Nuages.Elem[S]
    val icon        = raphaelIcon(raphael.Shapes.CloudWhite)
    val prefix      = "Nuages"
    def typeID      = _Nuages.typeID
    def hasMakeDialog   = true

    def mkListView[S <: Sys[S]](obj: _Nuages.Obj[S])(implicit tx: S#Tx): ListObjView[S] =
      new Nuages.Impl(tx.newHandle(obj), nameOption(obj))

    type Config[S <: evt.Sys[S]] = _String

    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def makeObj[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val peer = _Nuages[S]
      val elem = _Nuages.Elem(peer)
      val obj = Obj(elem)
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, _Nuages.Obj[S]],
                                  var nameOption: Option[_String])
      extends ListObjView /* .Nuages */[S]
      with ObjViewImpl.Impl[S]
      with ListObjViewImpl.NonEditable[S]
      with ListObjViewImpl.EmptyRenderer[S] {

      type E[~ <: evt.Sys[~]] = _Nuages.Elem[~]

      def icon    = Nuages.icon
      def prefix  = Nuages.prefix
      def typeID  = Nuages.typeID

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val frame = NuagesFolderFrameImpl(obj())
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

  //  cursor.step { implicit tx =>
  //    val elem      = create(tx)(value)
  //    val obj       = Obj(elem)
  //    obj.attr.name = name
  //    addObject(tpe, parentH(), obj)
  //  }

  def raphaelIcon(shape: Path2D => Unit): Icon = raphael.Icon(16)(shape)

  trait Impl[S <: Sys[S]] extends ObjView[S] {
    override def toString = s"ElementView.$prefix(name = $name)"

    def dispose()(implicit tx: S#Tx): Unit = ()
  }

  /** A trait that when mixed in provides `isViewable` and `openView` as non-op methods. */
  trait NonViewable[S <: Sys[S]] {
    def isViewable: _Boolean = false

    def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = None
  }
}
