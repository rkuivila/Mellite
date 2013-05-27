package de.sciss
package mellite
package gui

import de.sciss.lucre.stm
import de.sciss.synth.{ugen, SynthGraph, addToTail, proc}
import de.sciss.synth.proc.{Synth, Grapheme, Server, Artifact, Bounce, Sys}
import de.sciss.desktop.{DialogSource, OptionPane, FileDialog, Window}
import scala.swing.{ProgressBar, Swing, Alignment, Label, GridPanel, Orientation, BoxPanel, FlowPanel, ButtonGroup, RadioButton, CheckBox, Component, ComboBox, Button, TextField}
import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat, AudioFileType}
import java.io.File
import javax.swing.{SwingUtilities, JFormattedTextField, JSpinner, SpinnerNumberModel}
import de.sciss.span.{SpanLike, Span}
import Swing._
import de.sciss.audiowidgets.AxisFormat
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.mellite.Element.ArtifactLocation
import scala.util.control.NonFatal
import java.text.ParseException
import scala.swing.event.{ButtonClicked, SelectionChanged}
import scala.util.{Failure, Success, Try}
import de.sciss.processor.Processor
import de.sciss.synth.expr.{ExprImplicits, Doubles, Longs}
import de.sciss.file._

object ActionBounceTimeline {

  //  object GainType {
  //    def apply(id: Int): GainType = (id: @switch) match {
  //      case Normalized.id  => Normalized
  //      case Immediate .id  => Immediate
  //    }
  //  }
  //  sealed trait GainType { def id: Int }
  //  case object Normalized extends GainType { final val id = 0 }
  //  case object Immediate  extends GainType { final val id = 1 }

  private val DEBUG = false

  final case class QuerySettings[S <: Sys[S]](
    file: Option[File]  = None,
    spec: AudioFileSpec = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = 44100.0),
    gain: Gain = Gain.normalized(-0.2f),
    span: SpanOrVoid    = Span.Void,
    channels: IIdxSeq[Range.Inclusive] = Vector(0 to 0 /* 1 */),
    importFile: Boolean = false,
    location:  Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None,
    transform: Option[stm.Source[S#Tx, Element.Code    [S]]] = None
  ) {
    def prepare(group: stm.Source[S#Tx, proc.ProcGroup[S]], f: File): PerformSettings[S] = {
      val server                = Server.Config()
      specToServerConfig(f, spec, server)
      PerformSettings(
        group = group, server = server, gain = gain, span = span, channels = channels
      )
    }
  }

  final case class PerformSettings[S <: Sys[S]](
    group: stm.Source[S#Tx, proc.ProcGroup[S]],
    server: Server.Config,
    gain: Gain = Gain.normalized(-0.2f),
    span: SpanLike, channels: IIdxSeq[Range.Inclusive]
  )

  def specToServerConfig(file: File, spec: AudioFileSpec, config: Server.ConfigBuilder) {
    config.nrtOutputPath      = file.path
    config.nrtHeaderFormat    = spec.fileType
    config.nrtSampleFormat    = spec.sampleFormat
    config.sampleRate         = spec.sampleRate.toInt
    config.outputBusChannels  = spec.numChannels
  }

  type CodeSource[S <: Sys[S]] = stm.Source[S#Tx, Element.Code[S]]

  def findTransforms[S <: Sys[S]](document: Document[S])(implicit tx: S#Tx): IIdxSeq[Labeled[CodeSource[S]]] = {
    type Res = IIdxSeq[Labeled[CodeSource[S]]]
    def loop(xs: List[Element[S]], res: Res): Res =
      xs match {
        case (elem: Element.Code[S]) :: tail =>
          val res1 = elem.entity.value match {
            case ft: Code.FileTransform => res :+ Labeled(tx.newHandle(elem))(elem.name.value)
            case _ => res
          }
          loop(tail, res1)
        case (f: Element.Folder[S]) :: tail =>
          val res1 = loop(f.entity.iterator.toList, res)
          loop(tail, res1)
        case _ :: tail  => loop(tail, res)
        case Nil        => res
      }

    loop(document.elements.iterator.toList, Vector.empty)
  }

  def query[S <: Sys[S]](init: QuerySettings[S], document: Document[S], timelineModel: TimelineModel,
                         window: Option[Window])
                        (implicit cursor: stm.Cursor[S]) : (QuerySettings[S], Boolean) = {

    val ggFileType      = new ComboBox[AudioFileType](AudioFileType.writable)
    ggFileType.selection.item = init.spec.fileType // AudioFileType.AIFF
    val ggSampleFormat  = new ComboBox[SampleFormat](SampleFormat.fromInt16)
    GUI.fixWidth(ggSampleFormat)
    // ggSampleFormat.items = fuck you scala no method here
    ggSampleFormat.selection.item = init.spec.sampleFormat

    val ggPathText      = new TextField(32)

    def setPathText(file: File) {
      ggPathText.text = file.replaceExt(ggFileType.selection.item.extension).path
    }

    ggFileType.listenTo(ggFileType.selection)
    ggFileType.reactions += {
      case SelectionChanged(_) =>
        val s = ggPathText.text
        if (s != "") setPathText(new File(s))
    }

    init.file.foreach(f => ggPathText.text = f.path)
    val ggPathDialog    = Button("...") {
      val dlg = FileDialog.save(init = Some(new File(ggPathText.text)), title = "Bounce Audio Output File")
      dlg.show(window).foreach(setPathText)
    }
    ggPathDialog.peer.putClientProperty("JButton.buttonType", "gradient")

    val gainModel   = new SpinnerNumberModel(init.gain.decibels, -160.0, 160.0, 0.1)
    val ggGainAmtJ  = new JSpinner(gainModel)
    // println(ggGainAmtJ.getPreferredSize)
    val ggGainAmt   = Component.wrap(ggGainAmtJ)

    val ggGainType  = new ComboBox(Seq("Normalized", "Immediate"))
    ggGainType.selection.index = if (init.gain.normalized) 0 else 1
    ggGainType.listenTo(ggGainType.selection)
    ggGainType.reactions += {
      case SelectionChanged(_) =>
        ggGainType.selection.index match {
          case 0 => gainModel.setValue(-0.2)
          case 1 => gainModel.setValue( 0.0)
        }
        ggGainAmt.requestFocus()
    }

    val ggSpanAll   = new RadioButton("Automatic")
    val tlSel       = init.span match {
      case sp: Span => sp
      case _        => timelineModel.selection
    }
    val selectionText = tlSel match {
      case Span(start, stop)  =>
        val sampleRate  = timelineModel.sampleRate
        val fmt         = AxisFormat.Time()
        s"(${fmt.format(start/sampleRate)} ... ${fmt.format(stop/sampleRate)})"
      case _ =>
        ""
    }
    val ggSpanUser  = new RadioButton(s"Current Selection $selectionText")
    val ggSpanGroup = new ButtonGroup(ggSpanAll, ggSpanUser)

    ggSpanGroup.select(if (init.span.isEmpty) ggSpanAll else ggSpanUser)
    ggSpanUser.enabled   = tlSel.nonEmpty

    var transformItemsCollected = false

    def updateTransformEnabled() {
      val enabled = ggImport.selected
      checkTransform.enabled = enabled
      ggTransform   .enabled = enabled && checkTransform.selected
      if (ggTransform.enabled && !transformItemsCollected) {
        val trns = cursor.step { implicit tx =>
          findTransforms(document)
        }
        transformItemsCollected =  true
        ggTransform.peer.setModel(ComboBox.newConstantModel(trns))
        for (t <- init.transform; lb <- trns.find(_.value == t)) {
          ggTransform.selection.item = lb
        }
      }
    }

    lazy val ggImport  = new CheckBox() {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => updateTransformEnabled()
      }
    }
    ggImport.selected = init.importFile
    updateTransformEnabled()

    lazy val checkTransform = new CheckBox() {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => updateTransformEnabled()
      }
    }
    lazy val ggTransform = new ComboBox[Labeled[stm.Source[S#Tx, Element.Code[S]]]](Nil) // lazy filling
    val pTransform  = new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(checkTransform, HStrut(4), ggTransform)
    }

    // cf. stackoverflow #4310439 - with added spaces after comma
    // val regRanges = """^(\d+(-\d+)?)(,\s*(\d+(-\d+)?))*$""".r

    // cf. stackoverflow #16532768
    val regRanges = """(\d+(-\d+)?)""".r

    val fmtRanges = new JFormattedTextField.AbstractFormatter {
      def stringToValue(text: String): IIdxSeq[Range.Inclusive] = try {
        regRanges.findAllIn(text).toIndexedSeq match {
          case list if list.nonEmpty => list.map { s =>
            val i = s.indexOf('-')
            if (i < 0) {
              val a = s.toInt - 1
              a to a
            } else {
              val a = s.substring(0, i).toInt - 1
              val b = s.substring(i +1).toInt - 1
              (a to b)
            }
          }
        }
      } catch {
        case e @ NonFatal(_) =>
          e.printStackTrace()
          throw new ParseException(text, 0)
      }

      def valueToString(value: Any): String = try {
        value match {
          case sq: IIdxSeq[_] => sq.map {
            case r: Range if (r.start < r.end)  => s"${r.start + 1}-${r.end + 1}"
            case r: Range                       => s"${r.start + 1}"
          } .mkString(", ")
        }
      } catch {
        case NonFatal(_) => throw new ParseException(Option(value).map(_.toString).getOrElse("null"), 0)
      }
    }

    val ggChannelsJ = new JFormattedTextField(fmtRanges)
    ggChannelsJ.setColumns(12)
    ggChannelsJ.setValue(init.channels)
    ggChannelsJ.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT)
    val ggChannels  = Component.wrap(ggChannelsJ)

    import Swing.EmptyIcon
    import Alignment.Trailing
    val pPath     = new FlowPanel(ggPathText, ggPathDialog)
    val pFormat   = new FlowPanel(ggFileType, ggSampleFormat, ggGainAmt, new Label("dB"), ggGainType)
    val pSpan     = new GridPanel(6, 2) {
      contents ++= Seq(new Label("Channels:"                       , EmptyIcon, Trailing), ggChannels,
                       new Label("Timeline Span:"                  , EmptyIcon, Trailing), ggSpanAll,
                       HStrut(1),                                                          ggSpanUser,
                       HStrut(1),                                                          VStrut(32),
                       new Label("Import Output File Into Session:", EmptyIcon, Trailing), ggImport,
                       new Label("Apply Transformation:"           , EmptyIcon, Trailing), pTransform)
    }

    val box       = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(pPath, pFormat, pSpan)
    }

    val opt = OptionPane.confirmation(message = box, optionType = OptionPane.Options.OkCancel,
      messageType = OptionPane.Message.Plain)
    opt.title = "Bounce Timeline to Disk"
    val ok    = opt.show(window) == OptionPane.Result.Ok
    val file  = if (ggPathText.text == "") None else Some(new File(ggPathText.text))

    val channels: IIdxSeq[Range.Inclusive] = try {
      fmtRanges.stringToValue(ggChannelsJ.getText)
    } catch {
      case _: ParseException => init.channels
    }

    val importFile  = ggImport.selected
    val numChannels = channels.map(_.size).sum

    var settings = QuerySettings(
      file        = file,
      spec        = AudioFileSpec(ggFileType.selection.item, ggSampleFormat.selection.item,
        numChannels = numChannels, sampleRate = timelineModel.sampleRate),
      gain        = Gain(gainModel.getNumber.floatValue(), if (ggGainType.selection.index == 0) true else false),
      span        = if (ggSpanUser.selected) tlSel else Span.Void,
      channels    = channels,
      importFile  = importFile,
      location    = init.location,
      transform   = if (checkTransform.selected) Option(ggTransform.selection.item).map(_.value) else None
    )

    file match {
      case Some(f) if importFile =>
        init.location match {
          case Some(source) if cursor.step { implicit tx =>
              val parent = source().entity.directory
              Try(Artifact.relativize(parent, f)).isSuccess
            } =>  // ok, keep previous location

          case _ => // either no location was set, or it's not parent of the file
            ActionArtifactLocation.query(document, f) match {
              case res @ Some(_)  => settings = settings.copy(location = res)
              case _              => return (settings, false)
            }
        }
      case _ =>
    }

    if (ok && file.isEmpty) return query(settings, document, timelineModel, window)

    (settings, ok)
  }

  //  final case class QuerySettings[S <: Sys[S]](
  //    file: Option[File] = None,
  //    spec: AudioFileSpec = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = 44100.0),
  //    gainAmount: Double = -0.2, gainType: Gain = Normalized,
  //    span: SpanOrVoid = Span.Void, channels: IIdxSeq[Range.Inclusive] = Vector(0 to 1),
  //    importFile: Boolean = true, location: Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None
  //  )

  def performGUI[S <: Sys[S]](document: Document[S],
                              settings: QuerySettings[S],
                              group: stm.Source[S#Tx, proc.ProcGroup[S]], file: File,
                              window: Option[Window] = None)
                             (implicit cursor: stm.Cursor[S]) {

    val hasTransform= settings.importFile && settings.transform.isDefined
    val bounceFile  = if (hasTransform) {
      File.createTempFile("bounce", "." + settings.spec.fileType.extension)
    } else {
      file
    }
    val pSet        = settings.prepare(group, bounceFile)
    var process: Processor[Any, _] = perform(document, pSet)

    val ggProgress  = new ProgressBar()
    val ggCancel    = Button("Abort") {
      process.abort()
    }
    ggCancel.focusable = false
    val op    = OptionPane(message = ggProgress, messageType = OptionPane.Message.Plain, entries = Seq(ggCancel))
    val title = s"Bouncing to ${file.name} ..."
    op.title  = title

    var processCompleted = false

    def fDispose() {
      val w = SwingUtilities.getWindowAncestor(op.peer); if (w != null) w.dispose()
      processCompleted = true
    }
    val progDiv = if (hasTransform) 2 else 1
    process.addListener {
      case prog @ Processor.Progress(_, _) => GUI.defer(ggProgress.value = prog.toInt / progDiv)
    }

    val onFailure: PartialFunction[Any, Unit] = {
      case Failure(Processor.Aborted()) =>
        GUI.defer(fDispose())
      case Failure(e: Exception) => // XXX TODO: Desktop should allow Throwable for DialogSource.Exception
        GUI.defer {
          fDispose()
          DialogSource.Exception(e -> title).show(window)
        }
      case Failure(e) =>
        GUI.defer(fDispose())
        e.printStackTrace()
    }

    def bounceDone() {
      if (DEBUG) println(s"bounceDone(). hasTransform? $hasTransform")
      if (hasTransform) {
        val ftOpt = cursor.step { implicit tx =>
          settings.transform.flatMap(_.apply().entity.value match {
            case ft: Code.FileTransform => Some(ft)
            case _ => None
          })
        }
        if (DEBUG) println(s"file transform option = ${ftOpt.isDefined}")
        ftOpt match {
          case Some(ft) =>
            ft.execute((bounceFile, file, { codeProc =>
              if (DEBUG) println("code code processor")
              process = codeProc
              codeProc.addListener {
                case prog @ Processor.Progress(_, _) => GUI.defer(ggProgress.value = prog.toInt / progDiv + 50)
              }
              codeProc.onSuccess { case _ => allDone() }
              codeProc.onFailure(onFailure)
            }))
          case _ =>
            println("WARNING: Code does not denote a file transform")
            GUI.defer(fDispose())
            IO.revealInFinder(file)
        }

      } else {
        allDone()
      }
    }

    def allDone() {
      if (DEBUG) println("allDone")
      GUI.defer(fDispose())
      (settings.importFile, settings.location) match {
        case (true, Some(locSource)) =>
          val elemName  = file.base
          val spec      = AudioFile.readSpec(file)
          cursor.step { implicit tx =>
            val loc       = locSource()
            loc.entity.modifiableOption.foreach { locM =>
              val imp = ExprImplicits[S]
              import imp._
              // val fileR     = Artifact.relativize(locM.directory, file)
              // val artifact  = locM.add(file)
              // val depArtif  = Artifact.Modifiable(artifact)
              val depArtif  = locM.add(file)
              val depOffset = Longs  .newVar(0L)
              val depGain   = Doubles.newVar(1.0)
              val deployed  = Grapheme.Elem.Audio.apply(depArtif, spec, depOffset, depGain)
              val depElem   = Element.AudioGrapheme(file.base, deployed)
              val transfOpt = settings.transform.map(_.apply())
              val recursion = Recursion(group(), settings.span, depElem, settings.gain, settings.channels, transfOpt)
              val recElem   = Element.Recursion(elemName, recursion)
              document.elements.addLast(depElem)
              document.elements.addLast(recElem)
            }
          }

        case _ =>
          IO.revealInFinder(file)
      }
    }

    process.onSuccess { case _ => bounceDone() }
    process.onFailure(onFailure)

    GUI.delay(500) {
      if (!processCompleted) op.show(window)
    }
  }

  def perform[S <: Sys[S]](document: Document[S], settings: PerformSettings[S])
                          (implicit cursor: stm.Cursor[S]): Processor[File, _] = {
    import document.inMemoryBridge
    val bounce  = Bounce[S, document.I]
    val bnc     = bounce.Config()
    bnc.group   = settings.group
    bnc.server.read(settings.server)
    bnc.span    = settings.span
    bnc.init    = { (_tx, s) =>
      implicit val tx = _tx
      val graph = SynthGraph {
        import ugen._
        val inChans = settings.channels.flatten
        val mxChan  = inChans.max
        val sigIn   = In.ar(0, mxChan + 1 ) // XXX TODO: immediate gain could be applied here
        inChans.zipWithIndex.foreach { case (in, out) =>
          ReplaceOut.ar(out, sigIn \ in)
        }
      }
      Synth.play(graph)(s.defaultGroup, addAction = addToTail)
    }

    val process = bounce(bnc)
    process.start()
    process
  }
}