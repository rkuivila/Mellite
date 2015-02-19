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
import de.sciss.desktop.{FileDialog, OptionPane}
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.expr.{Boolean => BooleanEx, Double => DoubleEx, Expr, ExprType, Int => IntEx, Long => LongEx, String => StringEx}
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.{Window, deferTx}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.edit.{EditArtifactLocation, EditFolderInsertObj}
import de.sciss.mellite.gui.impl.document.NuagesFolderFrameImpl
import de.sciss.model.Change
import de.sciss.nuages.{NuagesView, ScissProcs}
import de.sciss.swingplus.{ComboBox, GroupPanel, Spinner}
import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.impl.{ElemImpl, FolderElemImpl}
import de.sciss.synth.proc.{ArtifactLocationElem, AudioGraphemeElem, BooleanElem, Confluent, DoubleElem, ExprImplicits, FolderElem, Grapheme, IntElem, LongElem, Obj, StringElem}
import de.sciss.{desktop, lucre}

import scala.swing.Swing.EmptyIcon
import scala.swing.{Alignment, CheckBox, Component, Dialog, Label, TextField}
import scala.util.Try

object ObjViewImpl {
  import java.lang.{String => _String}

  import de.sciss.lucre.artifact.{ArtifactLocation => _ArtifactLocation}
  import de.sciss.mellite.gui.ObjView.Factory
  import de.sciss.mellite.{Recursion => _Recursion}
  import de.sciss.nuages.{Nuages => _Nuages}
  import de.sciss.synth.proc.{Action => _Action, Code => _Code, Ensemble => _Ensemble, FadeSpec => _FadeSpec, Folder => _Folder, Proc => _Proc, Timeline => _Timeline}

  import scala.{Boolean => _Boolean, Double => _Double, Int => _Int, Long => _Long}

  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ObjView[S] = {
    val tid = obj.elem.typeID
    // getOrElse(sys.error(s"No view for type $tid"))
    map.get(tid).fold[ObjView[S]](Generic(obj))(f => f(obj.asInstanceOf[Obj.T[S, f.E]]))
  }

  private var map = scala.Predef.Map[_Int, Factory](
    String          .typeID -> String,
    Int             .typeID -> Int,
    Long            .typeID -> Long,
    Double          .typeID -> Double,
    Boolean         .typeID -> Boolean,
    AudioGrapheme   .typeID -> AudioGrapheme,
    ArtifactLocation.typeID -> ArtifactLocation,
    Recursion       .typeID -> Recursion,
    Folder          .typeID -> Folder,
    Proc            .typeID -> Proc,
    Timeline        .typeID -> Timeline,
    Code            .typeID -> Code,
    FadeSpec        .typeID -> FadeSpec,
    Action          .typeID -> Action,
    Ensemble        .typeID -> Ensemble,
    Nuages          .typeID -> Nuages
  )

  // -------- Generic --------

  object Generic {
    val icon = raphaelIcon(raphael.Shapes.No)

    def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ObjView[S] = {
      val name = obj.name
      new Generic.Impl(tx.newHandle(obj), name)
    }

    private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj[S]], var name: _String)
      extends ObjView[S] with NonEditable[S] with NonViewable[S] {

      def prefix: String = "Generic"
      def typeID: Int = 0

      def value: Any = ()

      def configureRenderer(label: Label): Component = label

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false

      def icon: Icon = Generic.icon
    }
  }

  // -------- String --------

  object String extends Factory {
    type E[S <: evt.Sys[S]] = StringElem[S]
    val icon    = raphaelIcon(raphael.Shapes.Font)
    val prefix  = "String"
    def typeID  = ElemImpl.String.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, StringElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new String.Impl(tx.newHandle(obj), name, value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: Sys[S]] = PrimitiveConfig[_String]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.text))
    }

    def make[S <: Sys[S]](config: (_String, _String))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(StringElem(StringEx.newVar(StringEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, StringElem]],
                                 var name: _String, var value: _String,
                                 override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ObjView.String[S]
      with ObjViewImpl.Impl[S]
      with SimpleExpr[S, _String]
      with StringRenderer {

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

  // -------- Int --------

  object Int extends Factory {
    type E[S <: evt.Sys[S]] = IntElem[S]
    val icon    = raphaelIcon(Shapes.IntegerNumbers)
    val prefix  = "Int"
    def typeID  = ElemImpl.Int.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, IntElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Int.Impl(tx.newHandle(obj), name, value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: Sys[S]] = PrimitiveConfig[_Int]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0, _Int.MinValue, _Int.MaxValue, 1)
      val ggValue   = new Spinner(model)
      primitiveConfig[S, _Int](window, tpe = prefix, ggValue = ggValue,
        prepare = Some(model.getNumber.intValue()))
    }

    def make[S <: Sys[S]](config: (String, _Int))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(IntElem(IntEx.newVar(IntEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, IntElem]],
                                  var name: _String, var value: _Int,
                                  override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ObjView.Int[S]
      with ObjViewImpl.Impl[S]
      with SimpleExpr[S, _Int]
      with StringRenderer
      /* with NonViewable[S] */ {

      def prefix  = Int.prefix
      def icon    = Int.icon
      def typeID  = Int.typeID

      def exprType = IntEx

      def expr(implicit tx: S#Tx) = obj().elem.peer

      def convertEditValue(v: Any): Option[_Int] = v match {
        case num: _Int  => Some(num)
        case s: _String => Try(s.toInt).toOption
      }

      def testValue(v: Any): Option[_Int] = v match {
        case i: _Int  => Some(i)
        case _        => None
      }
    }
  }

  // -------- Long --------

  object Long extends Factory {
    type E[S <: evt.Sys[S]] = LongElem[S]
    val icon    = raphaelIcon(Shapes.IntegerNumbers)  // XXX TODO
    val prefix  = "Long"
    def typeID  = ElemImpl.Long.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, LongElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Long.Impl(tx.newHandle(obj), name, value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: Sys[S]] = PrimitiveConfig[_Long]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0L, _Long.MinValue, _Long.MaxValue, 1L)
      val ggValue   = new Spinner(model)
      primitiveConfig[S, _Long](window, tpe = prefix, ggValue = ggValue, prepare = Some(model.getNumber.longValue()))
    }

    def make[S <: Sys[S]](config: (String, _Long))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(LongElem(LongEx.newVar(LongEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, LongElem]],
                                  var name: _String, var value: _Long,
                                  override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ObjView.Long[S]
      with ObjViewImpl.Impl[S]
      with SimpleExpr[S, _Long]
      with StringRenderer {

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

  object Double extends Factory {
    type E[S <: evt.Sys[S]] = DoubleElem[S]
    val icon    = raphaelIcon(Shapes.RealNumbers)
    val prefix  = "Double"
    def typeID  = ElemImpl.Double.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, DoubleElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Double.Impl(tx.newHandle(obj), name, value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: Sys[S]] = PrimitiveConfig[_Double]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val model     = new SpinnerNumberModel(0.0, _Double.NegativeInfinity, _Double.PositiveInfinity, 1.0)
      val ggValue   = new Spinner(model)
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = Some(model.getNumber.doubleValue))
    }

    def make[S <: Sys[S]](config: (String, _Double))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(DoubleElem(DoubleEx.newVar(DoubleEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]],
                                                   var name: _String, var value: _Double,
                                                   override val isEditable: _Boolean, val isViewable: _Boolean)
      extends ObjView.Double[S]
      with ObjViewImpl.Impl[S]
      with SimpleExpr[S, _Double]
      with StringRenderer {

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

  object Boolean extends Factory {
    type E[S <: evt.Sys[S]] = BooleanElem[S]
    val icon    = raphaelIcon(Shapes.BooleanNumbers)
    val prefix  = "Boolean"
    def typeID  = ElemImpl.Boolean.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, BooleanElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      val isViewable  = tx.isInstanceOf[Confluent.Txn]
      new Boolean.Impl(tx.newHandle(obj), name, value, isEditable = isEditable, isViewable = isViewable)
    }

    type Config[S <: Sys[S]] = PrimitiveConfig[_Boolean]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue = new CheckBox()
      primitiveConfig[S, _Boolean](window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.selected))
    }

    def make[S <: Sys[S]](config: (String, _Boolean))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val obj = Obj(BooleanElem(BooleanEx.newVar(BooleanEx.newConst[S](value))))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, BooleanElem]],
                                  var name: _String, var value: _Boolean,
                                  override val isEditable: _Boolean, val isViewable: Boolean)
      extends ObjView.Boolean[S]
      with ObjViewImpl.Impl[S]
      with BooleanExprLike[S] with SimpleExpr[S, _Boolean] {

      def prefix  = Boolean.prefix
      def icon    = Boolean.icon
      def typeID  = Boolean.typeID

      def expr(implicit tx: S#Tx): Expr[S, _Boolean] = obj().elem.peer
    }
  }

  // -------- AudioGrapheme --------

  object AudioGrapheme extends Factory {
    type E[S <: evt.Sys[S]] = AudioGraphemeElem[S]
    val icon    = raphaelIcon(raphael.Shapes.Music)
    val prefix  = "AudioGrapheme"
    def typeID  = ElemImpl.AudioGrapheme.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, AudioGraphemeElem])(implicit tx: S#Tx): ObjView[S] = {
      val name  = obj.name
      val value = obj.elem.peer.value
      new AudioGrapheme.Impl(tx.newHandle(obj), name, value)
    }

    final case class Config[S <: Sys[S]](file: File, spec: AudioFileSpec,
                                         location: Either[stm.Source[S#Tx, ArtifactLocationElem.Obj[S]], (String, File)])

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val dlg = FileDialog.open(init = None /* locViews.headOption.map(_.directory) */, title = "Add Audio File")
      dlg.setFilter(f => Try(AudioFile.identify(f).isDefined).getOrElse(false))
      val fOpt = dlg.show(window)

      fOpt.flatMap { f =>
        ActionArtifactLocation.query[S](workspace.rootH, file = f, window = window).map { location =>
          val spec = AudioFile.readSpec(f)
          Config(file = f, spec = spec, location = location)
        }
      }
    }

    def make[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
      val (list0: List[Obj[S]], loc /* : ArtifactLocationElem.Obj[S] */) = config.location match {
        case Left(source) => (Nil, source())
        case Right((name, directory)) =>
          val objLoc  = ActionArtifactLocation.create(name = name, directory = directory)
          (objLoc :: Nil, objLoc)
      }
      loc.elem.peer.modifiableOption.fold[List[Obj[S]]](list0) { locM =>
        val audioObj = ObjectActions.mkAudioFile(locM, config.file, config.spec)
        audioObj :: list0
      }
    }

    private val timeFmt = AxisFormat.Time(hours = false, millis = true)

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]],
                                  var name: _String, var value: Grapheme.Value.Audio)
      extends ObjView.AudioGrapheme[S] with ObjViewImpl.Impl[S] with NonEditable[S] {

      def prefix  = AudioGrapheme.prefix
      def icon    = AudioGrapheme.icon
      def typeID  = AudioGrapheme.typeID

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: Grapheme.Value.Audio) =>
          deferTx { value = now }
          true
        case _ => false
      }

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val frame = AudioFileFrame(obj())
        Some(frame)
      }

      def configureRenderer(label: Label): Component = {
        // ex. AIFF, stereo 16-bit int 44.1 kHz, 0:49.492
        val spec    = value.spec
        val smp     = spec.sampleFormat
        val isFloat = smp match {
          case SampleFormat.Float | SampleFormat.Double => "float"
          case _ => "int"
        }
        val chans   = spec.numChannels match {
          case 1 => "mono"
          case 2 => "stereo"
          case n => s"$n-chan."
        }
        val sr  = f"${spec.sampleRate/1000}%1.1f"
        val dur = timeFmt.format(spec.numFrames.toDouble / spec.sampleRate)

        // XXX TODO: add offset and gain information if they are non-default
        val txt    = s"${spec.fileType.name}, $chans ${smp.bitsPerSample}-$isFloat $sr kHz, $dur"
        label.text = txt
        label
      }
    }
  }

    // -------- ArtifactLocation --------

    object ArtifactLocation extends Factory {
      type E[S <: evt.Sys[S]] = ArtifactLocationElem[S]
      val icon    = raphaelIcon(raphael.Shapes.Location)
      val prefix  = "ArtifactStore"
      def typeID  = ElemImpl.ArtifactLocation.typeID

      def apply[S <: Sys[S]](obj: ArtifactLocationElem.Obj[S])(implicit tx: S#Tx): ObjView[S] = {
        val name      = obj.name
        val peer      = obj.elem.peer
        val value     = peer.directory
        val editable  = peer.modifiableOption.isDefined
        new ArtifactLocation.Impl(tx.newHandle(obj), name, value, isEditable = editable)
      }

      type Config[S <: Sys[S]] = PrimitiveConfig[File]

      def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] =
        ActionArtifactLocation.queryNew(window = window)

      def make[S <: Sys[S]](config: (_String, File))(implicit tx: S#Tx): List[Obj[S]] = {
        // ActionArtifactLocation.create(directory, _name, targetFolder)
        val (name, directory) = config
        val peer  = _ArtifactLocation[S](directory)
        val elem  = ArtifactLocationElem(peer)
        val obj   = Obj(elem)
        obj.name = name
        obj :: Nil
      }

      final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, ArtifactLocationElem.Obj[S]],
                                    var name: _String, var directory: File, val isEditable: _Boolean)
        extends ObjView.ArtifactLocation[S]
        with ObjViewImpl.Impl[S]
        with StringRenderer
        with NonViewable[S] {

        def icon    = ArtifactLocation.icon
        def prefix  = ArtifactLocation.prefix
        def typeID  = ArtifactLocation.typeID

        def value   = directory

        def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
          case _ArtifactLocation.Moved(_, Change(_, now)) =>
            deferTx { directory = now }
            true
          case _ => false
        }

        def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
          val dirOpt = value match {
            case s: String  => Some(file(s))
            case f: File    => Some(f)
            case _          => None
          }
          dirOpt.flatMap { newDir =>
            val loc = obj().elem.peer
            if (loc.directory == newDir) None else loc.modifiableOption.map { mod =>
              EditArtifactLocation(mod, newDir)
            }
          }
        }
      }
    }

  // -------- Recursion --------

  object Recursion extends Factory {
    type E[S <: evt.Sys[S]] = _Recursion.Elem[S]
    val icon    = raphaelIcon(raphael.Shapes.Quote)
    val prefix  = "Recursion"
    def typeID  = _Recursion.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, _Recursion.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name      = obj.name
      val value     = obj.elem.peer.deployed.elem.peer.artifact.value
      new Recursion.Impl(tx.newHandle(obj), name, value)
    }

    type Config[S <: Sys[S]] = Unit

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = None

    def make[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = Nil

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Recursion.Elem]],
                                  var name: _String, var deployed: File)
      extends ObjView.Recursion[S] with ObjViewImpl.Impl[S] with NonEditable[S] {

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

  object Folder extends Factory {
    type E[S <: evt.Sys[S]] = FolderElem[S]
    def icon    = UIManager.getIcon("Tree.openIcon")  // Swing.EmptyIcon
    val prefix  = "Folder"
    def typeID  = FolderElemImpl.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, FolderElem])(implicit tx: S#Tx): ObjView[S] = {
      val name  = obj.name
      new Folder.Impl(tx.newHandle(obj), name)
    }

    type Config[S <: Sys[S]] = _String

    def initDialog[S <: Sys[S]](workspace: Workspace[S],window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = "New Folder"
      val res = opt.show(window)
      res
    }

    def make[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val elem  = FolderElem(_Folder[S])
      val obj   = Obj(elem)
      val imp   = ExprImplicits[S]
      obj.name = name
      obj :: Nil
    }

    // XXX TODO: could be viewed as a new folder view with this folder as root
    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, FolderElem]], var name: _String)
      extends ObjView.Folder[S]
      with ObjViewImpl.Impl[S]
      with EmptyRenderer[S]
      with NonEditable[S] {

      def prefix  = Folder.prefix
      def icon    = Folder.icon
      def typeID  = Folder.typeID

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        val folderObj = obj()
        val nameView  = ExprView.name(folderObj)
        Some(FolderFrame(nameView, folderObj.elem.peer))
      }
    }
  }

  // -------- Proc --------

  object Proc extends Factory {
    type E[S <: evt.Sys[S]] = _Proc.Elem[S]
    val icon    = raphaelIcon(raphael.Shapes.Cogs)
    val prefix  = "Proc"
    def typeID  = ElemImpl.Proc.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, _Proc.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name  = obj.name
      new Proc.Impl(tx.newHandle(obj), name)
    }

    type Config[S <: Sys[S]] = _String

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def make[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val peer  = _Proc[S]
      val elem  = _Proc.Elem(peer)
      val obj   = Obj(elem)
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Proc.Elem]], var name: _String)
      extends ObjView.Proc[S]
      with ObjViewImpl.Impl[S]
      with EmptyRenderer[S]
      with NonEditable[S] {

      def icon    = Proc.icon
      def prefix  = Proc.prefix
      def typeID  = Proc.typeID

      def isViewable = true

      // currently this just opens a code editor. in the future we should
      // add a scans map editor, and a convenience button for the attributes
      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        import de.sciss.mellite.Mellite.compiler
        val frame = CodeFrame.proc(obj())
        Some(frame)
      }
    }
  }

  // -------- Timeline --------

  object Timeline extends Factory {
    type E[S <: evt.Sys[S]] = _Timeline.Elem[S]
    val icon    = raphaelIcon(raphael.Shapes.Ruler)
    val prefix  = "Timeline"
    def typeID  = _Timeline.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, _Timeline.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name  = obj.name
      new Timeline.Impl(tx.newHandle(obj), name)
    }

    type Config[S <: Sys[S]] = _String

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def make[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val peer = _Timeline[S] // .Modifiable[S]
      val elem = _Timeline.Elem(peer)
      val obj = Obj(elem)
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Timeline.Elem]], var name: _String)
      extends ObjView.Timeline[S]
      with ObjViewImpl.Impl[S]
      with EmptyRenderer[S]
      with NonEditable[S] {

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

  // -------- Code --------

  object Code extends Factory {
    type E[S <: evt.Sys[S]] = _Code.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.Code)
    val prefix          = "Code"
    def typeID          = _Code.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, _Code.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name    = obj.name
      val value   = obj.elem.peer.value
      new Code.Impl(tx.newHandle(obj), name, value)
    }

    type Config[S <: Sys[S]] = PrimitiveConfig[_Code]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val ggValue = new ComboBox(Seq(_Code.FileTransform.name, _Code.SynthGraph.name))
      primitiveConfig(window, tpe = prefix, ggValue = ggValue, prepare = ggValue.selection.index match {
        case 0 => Some(_Code.FileTransform(
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

        case 1 => Some(_Code.SynthGraph(
          """|val in   = ScanIn("in")
            |val sig  = in
            |ScanOut("out", sig)
            |""".stripMargin
        ))

        case _  => None
      })
    }

    def make[S <: Sys[S]](config: (String, _Code))(implicit tx: S#Tx): List[Obj[S]] = {
      val (name, value) = config
      val peer  = _Code.Expr.newVar[S](_Code.Expr.newConst(value))
      val obj   = Obj(_Code.Elem(peer))
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Code.Elem]],
                                  var name: _String, var value: _Code)
      extends ObjView.Code[S]
      with ObjViewImpl.Impl[S]
      with NonEditable[S] {

      def icon    = Code.icon
      def prefix  = Code.prefix
      def typeID  = Code.typeID

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        import de.sciss.mellite.Mellite.compiler
        val frame = CodeFrame(obj(), hasExecute = false)
        Some(frame)
      }

      def configureRenderer(label: Label): Component = {
        label.text = value.contextName
        label
      }
    }
  }

  // -------- FadeSpec --------

  object FadeSpec extends Factory {
    type E[S <: evt.Sys[S]] = _FadeSpec.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.Up)
    val prefix          = "FadeSpec"
    def typeID          = ElemImpl.FadeSpec.typeID

    def apply[S <: Sys[S]](obj: Obj.T[S, _FadeSpec.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name    = obj.name
      val value   = obj.elem.peer.value
      new FadeSpec.Impl(tx.newHandle(obj), name, value)
    }

    type Config[S <: Sys[S]] = Unit

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
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

    def make[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = Nil

    private val timeFmt = AxisFormat.Time(hours = false, millis = true)

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _FadeSpec.Elem]],
                                  var name: _String, var value: _FadeSpec)
      extends ObjView.FadeSpec[S]
      with ObjViewImpl.Impl[S]
      with NonEditable[S]
      with NonViewable[S] {

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

  // -------- Action --------

  object Action extends Factory {
    type E[S <: evt.Sys[S]] = _Action.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.Bolt)
    val prefix          = "Action"
    def typeID          = _Action.typeID

    def apply[S <: Sys[S]](obj: _Action.Obj[S])(implicit tx: S#Tx): ObjView[S] = {
      val name    = obj.name
      // val value   = obj.elem.peer.value
      new Action.Impl(tx.newHandle(obj), name)
    }

    type Config[S <: Sys[S]] = _String

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def make[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val peer = _Action.Var(_Action.empty[S])
      val elem = _Action.Elem(peer)
      val obj = Obj(elem)
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, _Action.Obj[S]],
                                  var name: _String)
      extends ObjView.Action[S]
      with ObjViewImpl.Impl[S]
      with NonEditable[S]
      with EmptyRenderer[S] {

      def icon    = Action.icon
      def prefix  = Action.prefix
      def typeID  = Action.typeID

      def isViewable = true

      def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
        import de.sciss.mellite.Mellite.compiler
        val frame = CodeFrame.action(obj())
        Some(frame)
      }
    }
  }

  // -------- Ensemble --------

  object Ensemble extends Factory {
    type E[S <: evt.Sys[S]] = _Ensemble.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.Cube2)
    val prefix          = "Ensemble"
    def typeID          = _Ensemble.typeID

    def apply[S <: Sys[S]](obj: _Ensemble.Obj[S])(implicit tx: S#Tx): ObjView[S] = {
      val name    = obj.name
      // val value   = obj.elem.peer.value
      val ens     = obj.elem.peer
      val playingEx = ens.playing
      val playing = playingEx.value
      val isEditable  = playingEx match {
        case Expr.Var(_)  => true
        case _            => false
      }
      new Ensemble.Impl(tx.newHandle(obj), name, playing = playing, isEditable = isEditable)
    }

    final case class Config[S <: Sys[S]](name: String, offset: Long, playing: Boolean)

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
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

    def make[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = {
      val folder    = _Folder[S] // XXX TODO - can we ask the user to pick one?
      val offset    = LongEx   .newVar(LongEx   .newConst[S](config.offset ))
      val playing   = BooleanEx.newVar(BooleanEx.newConst[S](config.playing))
      val elem      = _Ensemble.Elem(_Ensemble[S](folder, offset, playing))
      val obj       = Obj(elem)
      obj.name = config.name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, _Ensemble.Obj[S]],
                                  var name: _String, var playing: _Boolean, val isEditable: Boolean)
      extends ObjView.Ensemble[S]
      with ObjViewImpl.Impl[S]
      with BooleanExprLike[S] {

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

  object Nuages extends Factory {
    type E[S <: evt.Sys[S]] = _Nuages.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.CloudWhite)
    val prefix          = "Nuages"
    def typeID          = _Nuages.typeID

    def apply[S <: Sys[S]](obj: _Nuages.Obj[S])(implicit tx: S#Tx): ObjView[S] = {
      val name = obj.name
      new Nuages.Impl(tx.newHandle(obj), name)
    }

    type Config[S <: Sys[S]] = _String

    def initDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
      val opt = OptionPane.textInput(message = s"Enter initial ${prefix.toLowerCase} name:",
        messageType = OptionPane.Message.Question, initial = prefix)
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res
    }

    def make[S <: Sys[S]](name: _String)(implicit tx: S#Tx): List[Obj[S]] = {
      val peer = _Nuages[S]
      val elem = _Nuages.Elem(peer)
      val obj = Obj(elem)
      obj.name = name
      obj :: Nil
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, _Nuages.Obj[S]],
                                  var name: _String)
      extends ObjView.Nuages[S]
      with ObjViewImpl.Impl[S]
      with NonEditable[S]
      with EmptyRenderer[S] {

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
  }

  /** A trait that when mixed in provides `isEditable` and `tryEdit` as non-op methods. */
  trait NonEditable[S <: Sys[S]] {
    def isEditable: _Boolean = false

    def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = None
  }

  /** A trait that when mixed in provides `isViewable` and `openView` as non-op methods. */
  trait NonViewable[S <: Sys[S]] {
    def isViewable: _Boolean = false

    def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = None
  }

  trait EmptyRenderer[S <: Sys[S]] {
    def configureRenderer(label: Label): Component = label
    def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false
    def value: Any = ()
  }

  trait StringRenderer {
    def value: Any

    def configureRenderer(label: Label): Component = {
      label.text = value.toString
      label
    }
  }

  trait ExprLike[S <: Sys[S], A] {
    _: ObjView[S] =>

    protected var exprValue: A

    // def obj: stm.Source[S#Tx, Obj.T[S, Elem { type Peer = Expr[S, A] }]]

    protected def testValue       (v: Any): Option[A]
    protected def convertEditValue(v: Any): Option[A]

    protected def exprType: ExprType[A]

    protected def expr(implicit tx: S#Tx): Expr[S, A]

    def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] =
      convertEditValue(value).flatMap { newValue =>
        expr match {
          case Expr.Var(vr) =>
            vr() match {
              case Expr.Const(x) if x == newValue => None
              case _ =>
                // val imp = ExprImplicits[S]
                // import imp._
                // vr() = newValue
                implicit val ser    = exprType.serializer   [S]
                implicit val serVr  = exprType.varSerializer[S]
                val ed = EditVar.Expr(s"Change $prefix Value", vr, exprType.newConst[S](newValue))
                Some(ed)
            }

          case _ => None
        }
      }

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
      case Change(_, now) =>
        testValue(now).exists { valueNew =>
          deferTx {
            exprValue = valueNew
          }
          true
        }
      case _ => false
    }

    // XXX TODO - this is a quick hack for demo
    def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      workspace match {
        case cf: Workspace.Confluent =>
          // XXX TODO - all this casting is horrible
          implicit val ctx = tx.asInstanceOf[Confluent#Tx]
          implicit val ser = exprType.serializer[Confluent]
          val w = new WindowImpl[Confluent](ExprView.name[Confluent](obj().asInstanceOf[Obj[Confluent]],
              "History for '", "'")) {
            val view = ExprHistoryView(cf, expr.asInstanceOf[Expr[Confluent, A]])
            init()
          }
          Some(w.asInstanceOf[Window[S]])
        case _ => None
      }
    }
  }

  trait SimpleExpr[S <: Sys[S], A] extends ExprLike[S, A] with ObjView[S] {
    // _: ObjView[S] =>

    override def value: A
    protected def value_=(x: A): Unit

    protected def exprValue: A = value
    protected def exprValue_=(x: A): Unit = value = x
  }

  private final val ggCheckBox = new CheckBox()

  trait BooleanExprLike[S <: Sys[S]] extends ExprLike[S, _Boolean] {
    _: ObjView[S] =>

    def exprType = BooleanEx

    def convertEditValue(v: Any): Option[_Boolean] = v match {
      case num: _Boolean  => Some(num)
      case s: _String     => Try(s.toBoolean).toOption
    }

    def testValue(v: Any): Option[_Boolean] = v match {
      case i: _Boolean  => Some(i)
      case _            => None
    }

    def configureRenderer(label: Label): Component = {
      ggCheckBox.selected   = exprValue
      ggCheckBox.background = label.background
      ggCheckBox
    }
  }
}
