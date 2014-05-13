package de.sciss.mellite
package gui
package impl

import de.sciss.synth.proc.{ProcGroupElem, Elem, ExprImplicits, Artifact, Grapheme, AudioGraphemeElem, StringElem, DoubleElem, Obj, IntElem}
import javax.swing.{Icon, SpinnerNumberModel}
import de.sciss.synth.proc.impl.{FolderElemImpl, ElemImpl}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.expr.{Expr, ExprType}
import de.sciss.lucre.stm
import de.sciss.{desktop, mellite, lucre}
import scala.util.Try
import de.sciss.icons.raphael
import de.sciss.synth.proc
import javax.swing.undo.UndoableEdit
import de.sciss.lucre.swing.edit.EditVar
import de.sciss.lucre.swing.{deferTx, View}
import de.sciss.file._
import scala.swing.{Label, ComboBox, TextField, Component, Swing}
import de.sciss.mellite.impl.RecursionImpl.RecursionElemImpl
import de.sciss.mellite.impl.CodeImpl.CodeElemImpl
import de.sciss.swingplus.Spinner
import de.sciss.model.Change
import java.awt.geom.Path2D
import de.sciss.desktop.{OptionPane, FileDialog}
import de.sciss.synth.io.{SampleFormat, AudioFile}
import de.sciss.mellite.gui.edit.EditInsertObj
import de.sciss.lucre.{event => evt}
import proc.Implicits._
import de.sciss.audiowidgets.{AxisFormat, Axis}

object ObjViewImpl {
  import ObjView.Factory
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean}
  import mellite.{Recursion => _Recursion, Code => _Code}
  import proc.{Folder => _Folder, ProcGroup => _ProcGroup, ArtifactLocation => _ArtifactLocation}

  private val sync = new AnyRef

  def addFactory(f: Factory): Unit = sync.synchronized {
    val tid = f.typeID
    if (map.contains(tid)) throw new IllegalArgumentException(s"View factory for type $tid already installed")
    map += tid -> f
  }

  def factories: Iterable[Factory] = map.values

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ObjView[S] = {
    val f = map.get(obj.elem.typeID).getOrElse(sys.error(s"No view for object $obj")) // XXX TODO: provide generic view
    f(obj.asInstanceOf[Obj.T[S, f.E]])
  }

  private var map = scala.Predef.Map[_Int, Factory](
    String          .typeID -> String,
    Int             .typeID -> Int,
    Double          .typeID -> Double,
    AudioGrapheme   .typeID -> AudioGrapheme,
    ArtifactLocation.typeID -> ArtifactLocation,
    Recursion       .typeID -> Recursion,
    Folder          .typeID -> Folder,
    ProcGroup       .typeID -> ProcGroup,
    Code            .typeID -> Code
  )

  // -------- String --------

  object String extends Factory {
    type E[S <: evt.Sys[S]] = StringElem[S]
    val icon            = raphaelIcon(raphael.Shapes.Font)
    val prefix          = "String"
    def typeID          = ElemImpl.String.typeID
    type Init           = (_String, _String)

    def apply[S <: Sys[S]](obj: Obj.T[S, StringElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.attr.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      new String.Impl(tx.newHandle(obj), name, value, isEditable = isEditable)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val expr      = ExprImplicits[S]
      import expr._
      val ggValue   = new TextField(20)
      ggValue.text  = "Value"
      actionAddPrimitive(folderH, window, tpe = prefix, ggValue = ggValue, prepare = Some(ggValue.text)) {
        implicit tx => value => StringElem(lucre.expr.String.newVar(value))
      }
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, StringElem]],
                                                   var name: _String, var value: _String,
                                                   override val isEditable: _Boolean)
      extends ObjView.String[S]
      with ObjViewImpl.Impl[S]
      with ExprLike[S, _String]
      with StringRenderer
      with NonViewable[S] {

      def prefix  = String.prefix
      def icon    = String.icon

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
    val icon            = raphaelIcon(Shapes.IntegerNumbers)
    val prefix          = "Int"
    def typeID          = ElemImpl.Int.typeID
    type Init           = (_String, _Int)

    def apply[S <: Sys[S]](obj: Obj.T[S, IntElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.attr.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      new Int.Impl(tx.newHandle(obj), name, value, isEditable = isEditable)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val expr      = ExprImplicits[S]
      import expr._
      val model     = new SpinnerNumberModel(0, _Int.MinValue, _Int.MaxValue, 1)
      val ggValue   = new Spinner(model)
      actionAddPrimitive[S, _Int](folderH, window, tpe = prefix, ggValue = ggValue,
        prepare = Some(model.getNumber.intValue())) { implicit tx =>
          value => IntElem(lucre.expr.Int.newVar(value))
      }
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, IntElem]],
                                                   var name: _String, var value: _Int,
                                                   override val isEditable: _Boolean)
      extends ObjView.Int[S]
      with ObjViewImpl.Impl[S]
      with ExprLike[S, _Int]
      with StringRenderer
      with NonViewable[S] {

      def prefix = Int.prefix
      def icon   = Int.icon

      def exprType = lucre.expr.Int

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

  // -------- Double --------

  object Double extends Factory {
    type E[S <: evt.Sys[S]] = DoubleElem[S]
    val icon            = raphaelIcon(Shapes.RealNumbers)
    val prefix          = "Double"
    def typeID          = ElemImpl.Double.typeID
    type Init           = (_String, _Double)

    def apply[S <: Sys[S]](obj: Obj.T[S, DoubleElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.attr.name
      val ex          = obj.elem.peer
      val value       = ex.value
      val isEditable  = ex match {
        case Expr.Var(_)  => true
        case _            => false
      }
      new Double.Impl(tx.newHandle(obj), name, value, isEditable = isEditable)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val expr      = ExprImplicits[S]
      import expr._
      val model     = new SpinnerNumberModel(0.0, _Double.NegativeInfinity, _Double.PositiveInfinity, 1.0)
      val ggValue   = new Spinner(model)
      actionAddPrimitive(folderH, window, tpe = prefix, ggValue = ggValue,
        prepare = Some(model.getNumber.doubleValue)) { implicit tx =>
          value => DoubleElem(lucre.expr.Double.newVar(value))
      }
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]],
                                                   var name: _String, var value: _Double,
                                                   override val isEditable: _Boolean)
      extends ObjView.Double[S]
      with ObjViewImpl.Impl[S]
      with ExprLike[S, _Double]
      with StringRenderer
      with NonViewable[S] {

      def prefix  = Double.prefix
      def icon    = Double.icon

      def exprType = lucre.expr.Double

      def expr(implicit tx: S#Tx) = obj().elem.peer

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

  // -------- AudioGrapheme --------

  object AudioGrapheme extends Factory {
    type E[S <: evt.Sys[S]] = AudioGraphemeElem[S]
    val icon            = raphaelIcon(raphael.Shapes.Music)
    val prefix          = "AudioGrapheme"
    def typeID          = ElemImpl.AudioGrapheme.typeID
    type Init           = File

    def apply[S <: Sys[S]](obj: Obj.T[S, AudioGraphemeElem])(implicit tx: S#Tx): ObjView[S] = {
      val name        = obj.attr.name
      val value = obj.elem.peer.value
      new AudioGrapheme.Impl(tx.newHandle(obj), name, value)
    }


    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      // val locViews  = folderView.locations
      val dlg       = FileDialog.open(init = None /* locViews.headOption.map(_.directory) */, title = "Add Audio File")
      dlg.setFilter(f => Try(AudioFile.identify(f).isDefined).getOrElse(false))
      val fOpt = dlg.show(window)
      for {
        f         <- fOpt
        locSource <- (/* folderView.findLocation(f) */ ??? : Option[stm.Source[S#Tx, Obj.T[S, _ArtifactLocation.Elem]]])
      } yield {
        val spec          = AudioFile.readSpec(f)
        cursor.step { implicit tx =>
          ???
//          val loc = locSource()
//          loc.elem.peer.modifiableOption.foreach { locM =>
//            ObjectActions.addAudioFile(folderH, -1, locM, f, spec)
//          }
        }
      }
    }

    private val timeFmt = AxisFormat.Time(hours = false, millis = true)

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]],
                                  var name: _String, var value: Grapheme.Value.Audio)
      extends ObjView.AudioGrapheme[S] with ObjViewImpl.Impl[S] with NonEditable[S] {

      def prefix  = AudioGrapheme.prefix
      def icon    = AudioGrapheme.icon

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: Grapheme.Value.Audio) =>
          deferTx { value = now }
          true
        case _ => false
      }

      def isViewable = true

      def openView(document: Document[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[View[S]] = {
        val frame = AudioFileFrame(document, obj())
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
      type E[S <: evt.Sys[S]] = _ArtifactLocation.Elem[S]
      val icon            = raphaelIcon(raphael.Shapes.Location)
      val prefix          = "ArtifactStore"
      def typeID          = ElemImpl.ArtifactLocation.typeID
      type Init           = File

      def apply[S <: Sys[S]](obj: Obj.T[S, _ArtifactLocation.Elem])(implicit tx: S#Tx): ObjView[S] = {
        val name      = obj.attr.name
        val value     = obj.elem.peer.directory
        new ArtifactLocation.Impl(tx.newHandle(obj), name, value)
      }

      def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
        val query = ActionArtifactLocation.queryNew(window = window)
        query.map { case (directory, _name) =>
          cursor.step { implicit tx =>
            // ActionArtifactLocation.create(directory, _name, targetFolder)
            val peer  = _ArtifactLocation.Modifiable[S](directory)
            val elem  = _ArtifactLocation.Elem(peer)
            val obj   = Obj(elem)
            obj.attr.name = _name
            addObject(prefix, folderH(), obj)
          }
        }
      }

      final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _ArtifactLocation.Elem]],
                                                     var name: _String, var directory: File)
        extends ObjView.ArtifactLocation[S]
        with ObjViewImpl.Impl[S]
        with StringRenderer
        with NonEditable[S]
        with NonViewable[S] {

        def icon    = ArtifactLocation.icon
        def prefix  = ArtifactLocation.prefix
        def value   = directory

        def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = update match {
          case _ArtifactLocation.Moved(_, Change(_, now)) =>
            deferTx { directory = now }
            true
          case _ => false
        }
      }
    }

  // -------- Recursion --------

  object Recursion extends Factory {
    type E[S <: evt.Sys[S]] = _Recursion.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.Quote)
    val prefix          = "Recursion"
    def typeID          = RecursionElemImpl.typeID
    type Init           = Unit

    def apply[S <: Sys[S]](obj: Obj.T[S, _Recursion.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name      = obj.attr.name
      val value     = obj.elem.peer.deployed.elem.peer.artifact.value
      new Recursion.Impl(tx.newHandle(obj), name, value)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = None

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Recursion.Elem]],
                                  var name: _String, var deployed: File)
      extends ObjView.Recursion[S] with ObjViewImpl.Impl[S] with NonEditable[S] {

      def icon    = Recursion.icon
      def prefix  = Recursion.prefix
      def value   = deployed

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false

      def isViewable = true

      def openView(document: Document[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[View[S]] = {
        val frame = RecursionFrame(document, obj())
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
    type E[S <: evt.Sys[S]] = _Folder.Elem[S]
    val icon            = Swing.EmptyIcon
    val prefix          = "Folder"
    def typeID          = FolderElemImpl.typeID
    type Init           = _String

    def apply[S <: Sys[S]](obj: Obj.T[S, _Folder.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name  = obj.attr.name
      new Folder.Impl(tx.newHandle(obj), name)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val opt = OptionPane.textInput(message = "Enter initial folder name:",
        messageType = OptionPane.Message.Question, initial = "Folder")
      opt.title = "New Folder"
      val res = opt.show(window)
      res.map { name =>
        cursor.step { implicit tx =>
          val elem  = _Folder.Elem(_Folder[S])
          val obj   = Obj(elem)
          val imp   = ExprImplicits[S]
          import imp._
          obj.attr.name = name
          addObject(prefix, folderH(), obj)
        }
      }
    }

    // XXX TODO: could be viewed as a new folder view with this folder as root
    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Folder.Elem]], var name: _String)
      extends ObjView.Folder[S]
      with ObjViewImpl.Impl[S]
      with EmptyRenderer
      with NonEditable[S]
      with NonViewable[S] {

      def value = ()

      def prefix  = Folder.prefix
      def icon    = Folder.icon

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false  // element addition and removal is handled by folder view
    }
  }

  // -------- ProcGroup --------

  object ProcGroup extends Factory {
    type E[S <: evt.Sys[S]] = ProcGroupElem[S]
    val icon            = raphaelIcon(raphael.Shapes.Ruler)
    val prefix          = "ProcGroup"
    def typeID          = ElemImpl.ProcGroup.typeID
    type Init           = _String

    def apply[S <: Sys[S]](obj: Obj.T[S, ProcGroupElem])(implicit tx: S#Tx): ObjView[S] = {
      val name  = obj.attr.name
      new ProcGroup.Impl(tx.newHandle(obj), name)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val opt = OptionPane.textInput(message = "Enter initial group name:",
        messageType = OptionPane.Message.Question, initial = "Timeline")
      opt.title = s"New $prefix"
      val res = opt.show(window)
      res.map { name =>
        cursor.step { implicit tx =>
          val peer  = _ProcGroup.Modifiable[S]
          val elem  = ProcGroupElem(peer)
          val obj   = Obj(elem)
          obj.attr.name = name
          addObject(prefix, folderH(), obj)
        }
      }
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, ProcGroupElem]], var name: _String)
      extends ObjView.ProcGroup[S]
      with ObjViewImpl.Impl[S]
      with EmptyRenderer
      with NonEditable[S] {

      def value   = ()
      def icon    = ProcGroup.icon
      def prefix  = ProcGroup.prefix

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false

      def isViewable = true

      def openView(document: Document[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[View[S]] = {
        val frame = TimelineFrame[S](document, obj())
        Some(frame)
      }
    }
  }

  // -------- Code --------

  object Code extends Factory {
    type E[S <: evt.Sys[S]] = _Code.Elem[S]
    val icon            = raphaelIcon(raphael.Shapes.Code)
    val prefix          = "Code"
    def typeID          = CodeElemImpl.typeID
    type Init           = Unit

    def apply[S <: Sys[S]](obj: Obj.T[S, _Code.Elem])(implicit tx: S#Tx): ObjView[S] = {
      val name    = obj.attr.name
      val value   = obj.elem.peer.value
      new Code.Impl(tx.newHandle(obj), name, value)
    }

    def initDialog[S <: Sys[S]](folderH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val ggValue = new ComboBox(Seq(_Code.FileTransform.name, _Code.SynthGraph.name))
      actionAddPrimitive(folderH, window, tpe = prefix, ggValue = ggValue, prepare = ggValue.selection.index match {
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
          """|val in   = scan.In("in")
            |val sig  = in
            |scan.Out("out", sig)
            |""".stripMargin
        ))

        case _  => None
      }) { implicit tx =>
        value =>
          val peer  = Codes.newVar[S](Codes.newConst(value))
          _Code.Elem(peer)
      }
    }

    final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, _Code.Elem]],
                                                   var name: _String, var value: _Code)
      extends ObjView.Code[S]
      with ObjViewImpl.Impl[S]
      with NonEditable[S] {

      def icon    = Code.icon
      def prefix  = Code.prefix

      def isUpdateVisible(update: Any)(implicit tx: S#Tx): _Boolean = false

      def isViewable = true

      def openView(document: Document[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[View[S]] = {
        val frame = CodeFrame(document, obj())
        Some(frame)
      }

      def configureRenderer(label: Label): Component = {
        label.text = value.contextName
        label
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
    EditInsertObj[S](name, tx.newHandle(parent), idx, tx.newHandle(obj))
  }

  def actionAddPrimitive[S <: Sys[S], A](parentH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window],
                                         tpe: String, ggValue: Component, prepare: => Option[A])
                           (create: S#Tx => A => Elem[S])(implicit cursor: stm.Cursor[S]): Option[UndoableEdit] = {
    val nameOpt = GUI.keyValueDialog(value = ggValue, title = s"New $tpe", defaultName = tpe, window = window)
    for {
      name  <- nameOpt
      value <- prepare
    } yield {
      cursor.step { implicit tx =>
        val elem      = create(tx)(value)
        val obj       = Obj(elem)
        obj.attr.name = name
        addObject(tpe, parentH(), obj)
      }
    }
  }

  def raphaelIcon(shape: Path2D => Unit): Icon = raphael.Icon(16)(shape)

  trait Impl[S <: Sys[S]] extends ObjView[S] {
    override def toString = s"ElementView.$prefix(name = $name)"
  }

  /** A trait that when mixed in provides `isEditable` and `tryEdit` as non-op methods. */
  trait NonEditable[S <: Sys[S]] {
    def isEditable: Boolean = false

    def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = None
  }

  /** A trait that when mixed in provides `isViewable` and `openView` as non-op methods. */
  trait NonViewable[S <: Sys[S]] {
    def isViewable: Boolean = false

    def openView(document: Document[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[View[S]] = None
  }

  trait EmptyRenderer {
    def configureRenderer(label: Label): Component = label
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
    var value: A

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
            value = valueNew
          }
          true
        }
      case _ => false
    }
  }
}
