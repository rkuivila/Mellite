package de.sciss
package mellite
package gui

import de.sciss.lucre.stm
import de.sciss.synth.proc
import de.sciss.synth.proc.{Grapheme, Server, Artifact, Bounce, Sys}
import de.sciss.desktop.{DialogSource, OptionPane, FileDialog, Window}
import scala.swing.{ProgressBar, Swing, Alignment, Label, GridPanel, Orientation, BoxPanel, FlowPanel, ButtonGroup, RadioButton, CheckBox, Component, ComboBox, Button, TextField}
import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat, AudioFileType}
import java.io.File
import javax.swing.{SwingUtilities, JFormattedTextField, JSpinner, SpinnerNumberModel}
import de.sciss.span.{SpanLike, Span}
import Swing._
import de.sciss.audiowidgets.AxisFormat
import de.sciss.mellite.gui.impl.TimelineProcView
import scala.annotation.switch
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.mellite.Element.ArtifactLocation
import scala.util.control.NonFatal
import java.text.ParseException
import collection.breakOut
import scala.swing.event.{SelectionChanged, ValueChanged}
import scala.util.{Failure, Success, Try}
import scala.concurrent.{ExecutionContext, Future}
import de.sciss.processor.Processor
import de.sciss.synth.expr.{ExprImplicits, Doubles, Longs}

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

  final case class QuerySettings[S <: Sys[S]](
    file: Option[File]  = None,
    spec: AudioFileSpec = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = 44100.0),
    gain: Gain = Gain.normalized(-0.2f),
    span: SpanOrVoid    = Span.Void,
    channels: IIdxSeq[Range.Inclusive] = Vector(0 to 1),
    importFile: Boolean = false,
    location: Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None
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
      ggPathText.text = file.replaceExtension(ggFileType.selection.item.extension).path
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

    val ggImport    = new CheckBox("Import Output File Into Session")
    ggImport.selected = init.importFile

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

    val pPath     = new FlowPanel(ggPathText, ggPathDialog)
    val pFormat   = new FlowPanel(ggFileType, ggSampleFormat, ggGainAmt, new Label("dB"), ggGainType)
    val pSpan     = new GridPanel(3, 2) {
      contents ++= Seq(new Label("Channels:"     , Swing.EmptyIcon, Alignment.Right), ggChannels,
                       new Label("Timeline Span:", Swing.EmptyIcon, Alignment.Right), ggSpanAll,
                       new Label(""),                                                 ggSpanUser)
    }

    val box       = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(pPath, pFormat, pSpan, VStrut(32), ggImport)
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
      location    = init.location
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

    val pSet        = settings.prepare(group, file)
    val process     = perform(document, pSet)

    val ggProgress  = new ProgressBar()
    val ggCancel    = Button("Abort") {
      process.abort()
    }
    ggCancel.focusable = false
    val op    = OptionPane(message = ggProgress, messageType = OptionPane.Message.Plain, entries = Seq(ggCancel))
    val title = s"Bouncing to ${file.name} ..."
    op.title  = title

    def fDispose() {
      val w = SwingUtilities.getWindowAncestor(op.peer); if (w != null) w.dispose()
    }
    process.addListener {
      case prog @ Processor.Progress(_, _) => GUI.defer(ggProgress.value = prog.toInt)
    }

    process.onComplete {
      case Success(_) =>
        GUI.defer(fDispose())
        (settings.importFile, settings.location) match {
          case (true, Some(locSource)) =>
            val elemName  = file.nameWithoutExtension
            val spec      = AudioFile.readSpec(file)
            cursor.step { implicit tx =>
              val loc       = locSource()
              loc.entity.modifiableOption.foreach { locM =>
                val imp = ExprImplicits[S]
                import imp._
                // val fileR     = Artifact.relativize(locM.directory, file)
                val artifact  = locM.add(file)
                val depArtif  = Artifact.Modifiable(artifact)
                val depOffset = Longs  .newVar(0L)
                val depGain   = Doubles.newVar(1.0)
                val deployed  = Grapheme.Elem.Audio.apply(depArtif, spec, depOffset, depGain)
                val depElem   = Element.AudioGrapheme(file.nameWithoutExtension, deployed)

                val recursion = Recursion(group(), settings.span, depElem, settings.gain, settings.channels)
                val recElem   = Element.Recursion(elemName, recursion)
                document.elements.addLast(depElem)
                document.elements.addLast(recElem)
              }
            }

          case _ =>
            IO.revealInFinder(file)
        }
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

    GUI.delay(500) {
      if (!process.isCompleted) op.show(window)
    }
  }

  def perform[S <: Sys[S]](document: Document[S], settings: PerformSettings[S])
                          (implicit cursor: stm.Cursor[S]): Processor[File, _] = {
    import document.inMemory
    val bounce  = Bounce[S, document.I]
    val bCfg    = bounce.Config()
    bCfg.group  = settings.group
    bCfg.server.read(settings.server)
    bCfg.span   = settings.span
    val process = bounce(bCfg)
    process.start()
    process
  }
}