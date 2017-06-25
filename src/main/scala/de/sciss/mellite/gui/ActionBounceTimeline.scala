/*
 *  ActionBounceTimeline.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import java.io.{EOFException, File, IOException}
import java.text.ParseException
import javax.swing.{JFormattedTextField, SpinnerNumberModel, SwingUtilities}

import de.sciss.audiowidgets.TimeField
import de.sciss.desktop.{Desktop, DialogSource, FileDialog, OptionPane, PathField, Util, Window}
import de.sciss.file._
import de.sciss.lucre.artifact.{Artifact, ArtifactLocation}
import de.sciss.lucre.expr.{DoubleObj, LongObj}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Obj
import de.sciss.lucre.swing.{View, defer}
import de.sciss.lucre.synth.{Buffer, Server, Synth, Sys}
import de.sciss.mellite.gui.edit.EditFolderInsertObj
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorLike}
import de.sciss.span.Span
import de.sciss.span.Span.SpanOrVoid
import de.sciss.swingplus.{ComboBox, Labeled, Spinner, SpinnerComboBox}
import de.sciss.synth.io.{AudioFile, AudioFileType, SampleFormat}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{AudioCue, Bounce, Code, Folder, TimeRef, Timeline, Workspace}
import de.sciss.synth.{SynthGraph, addToTail}
import de.sciss.{desktop, equal, numbers, swingplus, synth}

import scala.collection.immutable.{IndexedSeq => Vec, Iterable => IIterable, Seq => ISeq}
import scala.concurrent.blocking
import scala.language.implicitConversions
import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, SelectionChanged, ValueChanged}
import scala.swing.{Alignment, BoxPanel, Button, ButtonGroup, CheckBox, Component, Dialog, FlowPanel, GridPanel, Label, Orientation, ProgressBar, Swing, TextField, ToggleButton}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

object ActionBounceTimeline {
  private[this] val DEBUG = false

  final val title = "Export as Audio File"

  def presetAllTimeline[S <: Sys[S]](tl: Timeline[S])(implicit tx: S#Tx): List[SpanPreset] = {
    val opt = for {
      start <- tl.firstEvent
      stop  <- tl.lastEvent
    } yield {
      SpanPreset(" All ", Span(start, stop))
    }
    opt.toList
  }

  object FileFormat {
    final case class PCM(tpe: AudioFileType = AudioFileType.AIFF, sampleFormat: SampleFormat = SampleFormat.Int24)
      extends FileFormat {

      def isPCM = true
    }

    final case class MP3(kbps: Int = 256, vbr: Boolean = false, title: String = "", artist: String = "",
                         comment: String = "")
      extends FileFormat {

      def isPCM = false
    }
  }
  sealed trait FileFormat {
    def isPCM: Boolean
  }

  def mkNumChannels(channels: Vec[Range.Inclusive]): Int = channels.map(_.size).sum

  final case class QuerySettings[S <: Sys[S]](
                                               file        : Option[File]          = None,
                                               fileFormat  : FileFormat            = FileFormat.PCM(),
                                               sampleRate  : Int                   = 44100,
                                               gain        : Gain                  = Gain.normalized(-0.2f),
                                               span        : SpanOrVoid            = Span.Void,
                                               channels    : Vec[Range.Inclusive]  = Vector(0 to 1),
                                               realtime    : Boolean               = false,
                                               fineControl : Boolean               = false,
                                               importFile  : Boolean               = false,
                                               location    : Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None
  ) {
    def prepare(group: IIterable[stm.Source[S#Tx, Obj[S]]], f: File)(mkSpan: => Span): PerformSettings[S] = {
      val sConfig = Server.Config()
      Mellite.applyAudioPrefs(sConfig, useDevice = realtime, pickPort = realtime)
      if (fineControl) sConfig.blockSize = 1
      val numChannels = mkNumChannels(channels)
      specToServerConfig(f, fileFormat, numChannels = numChannels, sampleRate = sampleRate, config = sConfig)
      val span1: Span = span match {
        case s: Span  => s
        case _        => mkSpan
      }
      PerformSettings(
        realtime = realtime, fileFormat = fileFormat,
        group = group, server = sConfig, gain = gain, span = span1, channels = channels
      )
    }
  }

  final case class PerformSettings[S <: Sys[S]](
    realtime    : Boolean,
    fileFormat  : FileFormat,
    group       : IIterable[stm.Source[S#Tx, Obj[S]]],
    server      : Server.Config,
    gain        : Gain = Gain.normalized(-0.2f),
    span        : Span,
    channels    : Vec[Range.Inclusive]
  )

  /** Note: header format and sample format are left unspecified if file format is not PCM. */
  def specToServerConfig(file: File, fileFormat: FileFormat, numChannels: Int, sampleRate: Int,
                         config: Server.ConfigBuilder): Unit = {
    config.nrtOutputPath      = file.path
    fileFormat match {
      case pcm: FileFormat.PCM =>
        config.nrtHeaderFormat    = pcm.tpe
        config.nrtSampleFormat    = pcm.sampleFormat
      case _ =>
    }
    config.sampleRate         = sampleRate
    config.outputBusChannels  = numChannels
    val numPrivate = Prefs.audioNumPrivate.getOrElse(Prefs.defaultAudioNumPrivate)
    config.audioBusChannels   = config.outputBusChannels + numPrivate
  }

  type CodeSource[S <: Sys[S]] = stm.Source[S#Tx, Code.Obj[S]]

  def findTransforms[S <: Sys[S]](document: Workspace[S])(implicit tx: S#Tx): Vec[Labeled[CodeSource[S]]] = {
    type Res = Vec[Labeled[CodeSource[S]]]
    def loop(xs: List[Obj[S]], res: Res): Res =
      xs match {
        case (objT: Code.Obj[S]) :: tail =>
          val res1 = objT.value match {
            case _: Code.FileTransform => res :+ Labeled(tx.newHandle(objT))(objT.name)
            case _ => res
          }
          loop(tail, res1)
        case (objT: Folder[S]) :: tail =>
          val res1 = loop(objT.iterator.toList, res)
          loop(tail, res1)
        case _ :: tail  => loop(tail, res)
        case Nil        => res
      }

    loop(document.rootH().iterator.toList, Vector.empty)
  }

  sealed trait Selection
  case object SpanSelection     extends Selection
  case object DurationSelection extends Selection
  case object NoSelection       extends Selection

  private object FileType {
    final case class PCM(peer: AudioFileType) extends FileType {
      override def toString: String = peer.toString

      def extension: String = peer.extension

      def isPCM = true
    }
    final case object MP3 extends FileType {
      override def toString: String = extension

      def extension: String = "mp3"

      def isPCM = false
    }
  }
  private sealed trait FileType {
    def extension: String

    def isPCM: Boolean
  }

  private final case class KBPS(value: Int) {
    override def toString = s"$value kbps"
  }

  private def mp3BitRates: Vec[KBPS] = Vector(
    KBPS( 64), KBPS( 80), KBPS( 96), KBPS(112), KBPS(128),
    KBPS(144), KBPS(160), KBPS(192), KBPS(224), KBPS(256), KBPS(320))

  final case class SpanPreset(name: String, value: Span) {
    override def toString: String = name
  }

  def query[S <: Sys[S]](view: ViewHasWorkspace[S] with View.Editable[S],
                         init: QuerySettings[S], selectionType: Selection,
                         spanPresets: ISeq[SpanPreset]): (QuerySettings[S], Boolean) = {

    import view.{cursor, undoManager, workspace => document}
    val window          = Window.find(view.component)
    import equal.Implicits._

    import FlowPanel.Alignment.Leading

    val sqFileType      = AudioFileType.writable.map(FileType.PCM) :+ FileType.MP3
    val ggFileType      = new ComboBox[FileType](sqFileType)
    ggFileType.selection.item = init.fileFormat match {
      case f: FileFormat.PCM => FileType.PCM(f.tpe)
      case _: FileFormat.MP3 => FileType.MP3
    }

    val ggPCMSampleFormat = new ComboBox[SampleFormat](SampleFormat.fromInt16)
    val pPCMSampleFormat  = new FlowPanel(Leading)(ggPCMSampleFormat, Swing.HGlue)

    val ggMP3BitRate  = new ComboBox[KBPS](mp3BitRates)
    val ggMP3VBR      = new CheckBox("VBR")
    val pMP3Rate      = new FlowPanel(Leading)(ggMP3BitRate, ggMP3VBR)
    val ggMP3Title    = new TextField(10)
    val ggMP3Artist   = new TextField(10)
    val ggMP3Comment  = new TextField(10)
    val pMP3Meta      = new FlowPanel(
      new Label("Title:"  ), ggMP3Title,
      new Label("Author:" ), ggMP3Artist,
      new Label("Comment:"), ggMP3Comment)

    val ggImport      = new CheckBox()

    Util.sameWidths(pPCMSampleFormat, pMP3Rate)

    def fileFormatVisibility(pack: Boolean): Unit = {
      val isMP3 = ggFileType.selection.item === FileType.MP3
      val isPCM = !isMP3
      if (!pack || pPCMSampleFormat.visible != isPCM) {
        pPCMSampleFormat  .visible = isPCM
        pMP3Rate          .visible = isMP3
        pMP3Meta          .visible = isMP3
        ggImport          .enabled = isPCM
        if (!isPCM) ggImport.selected = false

        if (pack) {
          val w = SwingUtilities.getWindowAncestor(ggFileType.peer)
          if (w != null) w.pack()
        }
      }
    }

    ggFileType.listenTo(ggFileType.selection)
    ggFileType.reactions += {
      case SelectionChanged(_) => fileFormatVisibility(pack = true)
    }

    fileFormatVisibility(pack = false)

    init.fileFormat match {
      case f: FileFormat.PCM =>
        ggPCMSampleFormat .selection.item   = f.sampleFormat
        ggMP3BitRate      .selection.item   = KBPS(256)

      case f: FileFormat.MP3 =>
        ggPCMSampleFormat .selection.item   = SampleFormat.Int24
        ggMP3BitRate      .selection.index  = mp3BitRates.indexWhere(_.value >= f.kbps)
        ggMP3Title        .text             = f.title
        ggMP3Artist       .text             = f.artist
        ggMP3Comment      .text             = f.comment
        ggMP3VBR          .selected         = f.vbr
    }

    val ggSampleRate = new SpinnerComboBox[Int](value0 = init.sampleRate,
      minimum = 1, maximum = TimeRef.SampleRate.toInt,
      step = 100, items = Seq(44100, 48000, 88200, 96000))

    val ggPath    = new PathField
    ggPath.mode   = FileDialog.Save
    ggPath.title  = "Audio Output File"

    def setPath(file: File): Unit =
      ggPath.value = file.replaceExt(ggFileType.selection.item.extension)

    ggFileType.listenTo(ggFileType.selection)
    ggFileType.reactions += {
      case SelectionChanged(_) =>
        val p = ggPath.value
        if (!p.path.isEmpty) setPath(p)
    }

    init.file.foreach(f => ggPath.value = f)

    val gainModel   = new SpinnerNumberModel(init.gain.decibels, -160.0, 160.0, 0.1)
    val ggGainAmt   = new Spinner(gainModel)

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

    val ggRealtime = new CheckBox()
    ggRealtime.selected = init.realtime
    val ggFineControl = new CheckBox()
    ggFineControl.selected = init.fineControl

    // cf. stackoverflow #4310439 - with added spaces after comma
    // val regRanges = """^(\d+(-\d+)?)(,\s*(\d+(-\d+)?))*$""".r

    // cf. stackoverflow #16532768
    val regRanges = """(\d+(-\d+)?)""".r

    val fmtRanges = new JFormattedTextField.AbstractFormatter {
      def stringToValue(text: String): Vec[Range.Inclusive] = try {
        regRanges.findAllIn(text).toIndexedSeq match {
          case list if list.nonEmpty => list.map { s =>
            val i = s.indexOf('-')
            if (i < 0) {
              val a = s.toInt - 1
              a to a
            } else {
              val a = s.substring(0, i).toInt - 1
              val b = s.substring(i +1).toInt - 1
              a to b
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
          case sq: Vec[_] => sq.map {
            case r: Range if r.start < r.end  => s"${r.start + 1}-${r.end + 1}"
            case r: Range                     => s"${r.start + 1}"
          } .mkString(", ")
        }
      } catch {
        case NonFatal(_) => throw new ParseException(Option(value).fold("null")(_.toString), 0)
      }
    }

    val ggChannelsJ = new JFormattedTextField(fmtRanges)
    ggChannelsJ.setColumns(12)
    ggChannelsJ.setValue(init.channels)
    ggChannelsJ.setFocusLostBehavior(JFormattedTextField.COMMIT_OR_REVERT)
    val ggChannels  = Component.wrap(ggChannelsJ)
    ggChannels.tooltip = "Ranges of channels to bounce, such as 1-4 or 1,3,5"

    import Alignment.Trailing
    import Swing.EmptyIcon
    val pPath     = new FlowPanel(Leading)(new Label("Output File:"), ggPath)
    val pFormat   = new FlowPanel(Leading)(new Label("Format:"), ggFileType, pPCMSampleFormat, pMP3Rate, ggSampleRate)
    val pParams   = new GridPanel(0, 2)
    pParams.hGap  = 4
    pParams.contents ++= Seq(new Label("Gain:", EmptyIcon, Trailing),
      new FlowPanel(Leading)(ggGainAmt, new Label("dB"), ggGainType))
    pParams.contents ++= Seq(new Label("Channels:", EmptyIcon, Trailing), ggChannels)

    val span0F          = init.span.nonEmptyOption.getOrElse {
      spanPresets.headOption.fold(Span(0L, (10 * TimeRef.SampleRate).toLong))(_.value)
    }
    val ggSpanStart     = new TimeField(span0F.start, Span.Void, sampleRate = TimeRef.SampleRate, viewSampleRate0 = init.sampleRate)
    val ggSpanStopOrDur = new TimeField(span0F.stop , Span.Void, sampleRate = TimeRef.SampleRate, viewSampleRate0 = init.sampleRate)
    val ggSpanPresets   = spanPresets.map { pst =>
      new ToggleButton(pst.name) {
        listenTo(this)
        reactions += {
          case ButtonClicked(_) if selected =>
            if (selectionType === SpanSelection) {
              ggSpanStart     .value = pst.value.start
              ggSpanStopOrDur .value = pst.value.stop
            } else {
              ggSpanStopOrDur .value = pst.value.length
            }
            ggSpanStopOrDur.requestFocus()
        }
      }
    }
    val bgSpanPresets = new ButtonGroup(ggSpanPresets: _*)

    def mkSpan(): SpanOrVoid = selectionType match {
      case SpanSelection =>
        val a = ggSpanStart     .value
        val b = ggSpanStopOrDur .value
        Span(math.min(a, b), math.max(a, b))

      case DurationSelection =>
        val a = 0L
        val b = ggSpanStopOrDur.value
        Span(math.min(a, b), math.max(a, b))

      case NoSelection =>
        init.span
    }

    ggSampleRate.listenTo(ggSampleRate.spinner)
    ggSampleRate.reactions += {
      case ValueChanged(_) =>
        val sr = ggSampleRate.value
        ggSpanStart     .viewSampleRate = sr
        ggSpanStopOrDur .viewSampleRate = sr
    }

    def updatePresetSelection(): Unit = {
      val sp  = mkSpan()
      val idx = if (selectionType === SpanSelection) {
        spanPresets.indexWhere(_.value === sp)
      } else if (selectionType === DurationSelection) {
        val len = sp.length
        spanPresets.indexWhere(_.value.length == len)
      } else -1

      import swingplus.Implicits._
      if (idx < 0) bgSpanPresets.clearSelection() else bgSpanPresets.select(ggSpanPresets(idx))
    }

    if (selectionType === SpanSelection) {
      GUI.linkFormats(ggSpanStart, ggSpanStopOrDur)
      val pSpan = new FlowPanel(Leading)(ggSpanStart, new Label("–"), ggSpanStopOrDur)
      pSpan.contents ++= ggSpanPresets
      pParams.contents ++= Seq(new Label("Time Span:", EmptyIcon, Trailing), pSpan)
      ggSpanStart.listenTo(ggSpanStart)
      ggSpanStart.reactions += {
        case ValueChanged(_) => updatePresetSelection()
      }
      ggSpanStopOrDur.listenTo(ggSpanStopOrDur)
      ggSpanStopOrDur.reactions += {
        case ValueChanged(_) => updatePresetSelection()
      }

    } else if (selectionType === DurationSelection) {
      val pDur = new FlowPanel(Leading)(ggSpanStopOrDur)
      pDur.contents ++= ggSpanPresets
      pParams.contents ++= Seq(new Label("Duration:", EmptyIcon, Trailing), pDur)
      ggSpanStopOrDur.listenTo(ggSpanStopOrDur)
      ggSpanStopOrDur.reactions += {
        case ValueChanged(_) => updatePresetSelection()
      }
    }

    updatePresetSelection()

    pParams.contents ++= Seq(
      HStrut(1), VStrut(32),
      new Label("Run in Real-Time:" , EmptyIcon, Trailing), ggRealtime,
      new Label("Fine Control Rate:", EmptyIcon, Trailing), ggFineControl
    )
    ggImport.selected = init.importFile
    pParams.contents ++= Seq(new Label("Import into Workspace:", EmptyIcon, Trailing), ggImport)

    val box       = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(pPath, pFormat, pMP3Meta, pParams)
    }

    val opt = OptionPane.confirmation(message = box, optionType = OptionPane.Options.OkCancel,
      messageType = OptionPane.Message.Plain)
    opt.title = title
    val ok    = opt.show(window) === OptionPane.Result.Ok
    val file  = ggPath.valueOption

    val channels: Vec[Range.Inclusive] = try {
      fmtRanges.stringToValue(ggChannelsJ.getText)
    } catch {
      case _: ParseException => init.channels
    }

    val importFile  = ggImport.selected
    val spanOut     = mkSpan()
    val sampleRate  = ggSampleRate.value

    val fileFormat: FileFormat = ggFileType.selection.item match {
      case FileType.PCM(tpe) =>
        FileFormat.PCM(tpe, ggPCMSampleFormat.selection.item)
      case FileType.MP3 =>
        FileFormat.MP3(kbps = ggMP3BitRate.selection.item.value,
          vbr = ggMP3VBR.selected, title = ggMP3Title.text, artist = ggMP3Artist.text, comment = ggMP3Comment.text)
    }

    var settings = QuerySettings(
      realtime    = ggRealtime   .selected,
      fineControl = ggFineControl.selected,
      file        = file,
      fileFormat  = fileFormat,
      sampleRate  = sampleRate,
      gain        = Gain(gainModel.getNumber.floatValue(), if (ggGainType.selection.index == 0) true else false),
      span        = spanOut,
      channels    = channels,
      importFile  = importFile,
      location    = init.location
    )

    file match {
      case Some(f) if importFile =>

        init.location match {
          case Some(source) if cursor.step { implicit tx =>
              val parent = source().directory
              Try(Artifact.relativize(parent, f)).isSuccess
            } =>  // ok, keep previous location

          case _ => // either no location was set, or it's not parent of the file
            ActionArtifactLocation.query[S](document.rootH, f) match {
              case Some(either) =>
                either match {
                  case Left(source) =>
                    settings = settings.copy(location = Some(source))

                  case Right((name, directory)) =>
                    val (edit, source) = cursor.step { implicit tx =>
                      val locObj  = ActionArtifactLocation.create(name = name, directory = directory)
                      val folder  = document.rootH()
                      val index   = folder.size
                      val _edit   = EditFolderInsertObj("Location", folder, index, locObj)
                      (_edit, tx.newHandle(locObj))
                    }
                    undoManager.add(edit)
                    settings = settings.copy(location = Some(source))
                }

              case _ => return (settings, false)
            }
        }
      case _ =>
    }

    file match {
      case Some(f) if ok && f.exists() =>
        val ok1 = Dialog.showConfirmation(
          message     = s"<HTML><BODY>File<BR><B>${f.path}</B><BR>already exists.<P>Are you sure you want to overwrite it?</BODY>",
          title       = title,
          optionType  = Dialog.Options.OkCancel,
          messageType = Dialog.Message.Warning
        ) === Dialog.Result.Ok
        (settings, ok1)

      case None if ok =>
        query(view, settings, selectionType, spanPresets)
      case _ =>
        (settings, ok)
    }
  }

  def performGUI[S <: Sys[S]](view: ViewHasWorkspace[S],
                              settings: QuerySettings[S],
                              group: IIterable[stm.Source[S#Tx, Obj[S]]], file: File, span: Span): Unit = {

    import view.{cursor, workspace => document}
    val window      = Window.find(view.component)
    val bounceFile  = file
    val pSet        = settings.prepare(group, bounceFile)(span)
    val process: ProcessorLike[Any, Any] = perform(document, pSet)

    var processCompleted = false

    val ggProgress  = new ProgressBar()
    val ggCancel = new Button("Abort")
    ggCancel.focusable = false
    lazy val op = OptionPane(message = ggProgress, messageType = OptionPane.Message.Plain, entries = Seq(ggCancel))
    val title   = s"Exporting to ${file.name} ..."
    op.title    = title

    ggCancel.listenTo(ggCancel)
    ggCancel.reactions += {
      case ButtonClicked(_) =>
        process.abort()
        // currently process doesn't seem to abort under certain errors
        // (e.g. buffer allocator exhausted). XXX TODO
        val run = new Runnable { def run(): Unit = { Thread.sleep(1000); defer(fDispose()) }}
        new Thread(run).start()
    }

    def fDispose(): Unit = {
      val w = SwingUtilities.getWindowAncestor(op.peer); if (w != null) w.dispose()
      processCompleted = true
    }
    process.addListener {
      case prog @ Processor.Progress(_, _) => defer(ggProgress.value = prog.toInt)
    }

    val onFailure: PartialFunction[Throwable, Unit] = {
      case Processor.Aborted() =>
        defer(fDispose())
      case ex =>
        defer {
          fDispose()
          DialogSource.Exception(ex -> title).show(window)
        }
    }

    def bounceDone(): Unit = {
      if (DEBUG) println("allDone")
      defer(fDispose())
      (settings.importFile, settings.location) match {
        case (true, Some(locSource)) =>
          val spec      = AudioFile.readSpec(file)
          cursor.step { implicit tx =>
            val loc       = locSource()
            val depArtif  = Artifact(loc, file)
            val depOffset = LongObj  .newVar[S](0L)
            val depGain   = DoubleObj.newVar[S](1.0)
            val deployed  = AudioCue.Obj[S](depArtif, spec, depOffset, depGain)
            deployed.name = file.base
            document.rootH().addLast(deployed)
          }

        case _ =>
          Desktop.revealFile(file)
      }
    }

    process.onComplete {
      case Success(_)   => bounceDone()
      case Failure(ex)  => onFailure(ex)
    }
//    process.onSuccess { case _ => bounceDone() }
//    process.onFailure(onFailure)

    desktop.Util.delay(500) {
      if (!processCompleted) op.show(window)
    }
  }

  def perform[S <: Sys[S]](document: Workspace[S], settings: PerformSettings[S])
                          (implicit cursor: stm.Cursor[S]): Processor[File] = {
    implicit val workspace = document

    // for real-time, we generally have to overshoot because in SC 3.6, DiskOut's
    // buffer is not flushed after synth is stopped.
    val realtime      = settings.realtime
    val normalized    = settings.gain.normalized
    val compressed    = !settings.fileFormat.isPCM
    val needsTemp     = realtime || normalized || compressed
    val numChannels   = settings.server.outputBusChannels

    val span          = settings.span
    val fileOut       = file(settings.server.nrtOutputPath)
    val sampleRate    = settings.server.sampleRate
    val fileFrames0   = (span.length * sampleRate / TimeRef.SampleRate + 0.5).toLong
    val fileFrames    = fileFrames0 // - (fileFrames0 % settings.server.blockSize)

    val settings1: PerformSettings[S] = if (!needsTemp) settings else {
      val fTmp    = File.createTempFile("bounce", ".w64")
      fTmp.deleteOnExit()
      val sConfig = Server.ConfigBuilder(settings.server)
      sConfig.nrtOutputPath   = fTmp.path
      sConfig.nrtHeaderFormat = AudioFileType.Wave64
      sConfig.nrtSampleFormat = SampleFormat.Float
      // if (realtime) sConfig.outputBusChannels = 0
      settings.copy(server = sConfig)
    }

    val bounce    = Bounce[S, document.I]
    val bnc       = Bounce.Config[S]
    bnc.group     = settings1.group
    bnc.realtime  = realtime
    bnc.server.read(settings1.server)

    val bncGainFactor = if (settings1.gain.normalized) 1f else settings1.gain.linear
    val inChans       = settings1.channels.flatten
    val numInChans    = if (inChans.isEmpty) 0 else inChans.max + 1
    assert(numInChans >= numChannels)

    val span1 = if (!realtime) span else {
      val bufDur    = Buffer.defaultRecBufferSize.toDouble / bnc.server.sampleRate
      // apart from DiskOut buffer, add a bit of head-room (100ms) to account for jitter
      val bufFrames = ((bufDur + 0.1) * TimeRef.SampleRate + 0.5).toLong
      val numFrames = span.length + bufFrames // (span.length + bufFrames - 1) / bufFrames * bufFrames
      Span(span.start, span.start + numFrames)
    }
    bnc.span    = span1
    bnc.beforePrepare = { (_tx, s) =>
      implicit val tx = _tx
      // make sure no private bus overlaps with the virutal output
      if (numInChans > numChannels) {
        s.allocAudioBus(numInChans - numChannels)
      }
      val graph = SynthGraph {
        import synth._
        import ugen._
        val sigIn   = In.ar(0, numInChans)
        val sigOut: GE = inChans.map { in =>
          val chIn = sigIn \ in
          chIn * bncGainFactor
        }
        ReplaceOut.ar(0, sigOut)
      }

      // on Linux, scsynth in real-time connects to jack,
      // but the -H switch doesn't seem to work. So we end
      // up with a sample-rate determined by Jack and not us
      // (see https://github.com/Sciss/Mellite/issues/30).
      // As a clumsy work-around, we abort the bounce if we
      // see that the rate is incorrect.
      if (s.sampleRate != sampleRate) {
        throw new IOException(
          s"Real-time bounce - SuperCollider failed to set requested sample-rate of $sampleRate Hz. " +
          "Use a matching sample-rate, or try disabling real-time.")
      }

      Synth.play(graph)(s.defaultGroup, addAction = addToTail)
    }

    val bProcess  = bounce.apply(bnc)
    // bProcess.addListener {
    //   case u => println(s"UPDATE: $u")
    // }
    bProcess.start()
    val process   = if (!needsTemp) bProcess else {
      val nProcess = new Normalizer(bounce = bProcess,
        fileOut = fileOut, fileFormat = settings.fileFormat,
        gain = if (normalized) settings1.gain else Gain.immediate(0f), numFrames = fileFrames)
      nProcess.start()
      nProcess
    }
    process
  }

  // XXX TODO --- could use filtered console output via Poll to
  // measure max gain already during bounce
  private final class Normalizer[S <: Sys[S]](bounce: Processor[File],
                                              fileOut: File, fileFormat: FileFormat,
                                              gain: Gain, numFrames: Long)
    extends ProcessorImpl[File, Processor[File]] with Processor[File] {

    override def toString = s"Normalize bounce $fileOut"

    def body(): File = blocking {
      val fileIn  = await(bounce, weight = if (gain.normalized) 0.8 else 0.9)   // arbitrary weight

      // tricky --- scsynth flush might not yet be seen
      // thus wait a few seconds until header becomes available
      val t0 = System.currentTimeMillis()
      while (AudioFile.readSpec(fileIn).numFrames == 0L && System.currentTimeMillis() - t0 < 4000) {
        blocking(Thread.sleep(500))
      }

      val afIn = AudioFile.openRead(fileIn)

      import numbers.Implicits._
      try {
        val bufSz       = 8192
        val buf         = afIn.buffer(bufSz)
        val numFrames0  = math.min(afIn.numFrames, numFrames) // whatever...
        var rem         = numFrames0
//        if (rem >= afIn.numFrames)
//          throw new EOFException(s"Bounced file is too short (${afIn.numFrames} -- expected at least $rem)")
        if (afIn.numFrames == 0)
          throw new EOFException("Exported file is empty")

        val mul = if (!gain.normalized) gain.linear else {
          var max = 0f
          while (rem > 0) {
            val chunk = math.min(bufSz, rem).toInt
            afIn.read(buf, 0, chunk)
            var ch = 0; while (ch < buf.length) {
              val cBuf = buf(ch)
              var i = 0; while (i < chunk) {
                val f = math.abs(cBuf(i))
                if (f > max) max = f
                i += 1
              }
              ch += 1
            }
            rem -= chunk
            progress = (rem.toDouble / numFrames0).linlin(0, 1, 0.9, 0.8)
            checkAborted()
          }
          afIn.seek(0L)
          if (max == 0) 1f else gain.linear / max
        }

        def writePCM(f: File, tpe: AudioFileType, sampleFormat: SampleFormat, maxChannels: Int,
                     progStop: Double): Unit = {
          val numCh = if (maxChannels <= 0) afIn.numChannels else math.min(afIn.numChannels, maxChannels)
          val afOut = AudioFile.openWrite(f,
            afIn.spec.copy(fileType = tpe, numChannels = numCh, sampleFormat = sampleFormat, byteOrder = None))
          try {
            rem = numFrames0
            while (rem > 0) {
              val chunk = math.min(bufSz, rem).toInt
              afIn.read(buf, 0, chunk)
              if (mul != 1) {
                var ch = 0; while (ch < numCh) {
                  val cBuf = buf(ch)
                  var i = 0; while (i < chunk) {
                    cBuf(i) *= mul
                    i += 1
                  }
                  ch += 1
                }
              }
              afOut.write(buf, 0, chunk)
              rem -= chunk
              progress = (rem.toDouble / numFrames0).linlin(0, 1, progStop, 0.9)
              checkAborted()
            }
            afOut.close()
            afIn .close()
          } finally {
            if (afOut.isOpen) afOut.cleanUp()
          }
        }


        fileFormat match {
          case FileFormat.PCM(fileType, sampleFormat) =>
            writePCM(fileOut, fileType, sampleFormat, maxChannels = 0, progStop = 1.0)
          case mp3: FileFormat.MP3 =>
            val fTmp = File.createTempFile("bounce", ".aif")
            fTmp.deleteOnExit()
            try {
              writePCM(fTmp, AudioFileType.AIFF, SampleFormat.Int16, maxChannels = 2, progStop = 0.95)
              var lameArgs = List[String](fTmp.path, fileOut.path)
              if (!mp3.comment.isEmpty) lameArgs :::= List("--tc", mp3.comment)
              if (!mp3.artist .isEmpty) lameArgs :::= List("--ta", mp3.artist )
              if (!mp3.title  .isEmpty) lameArgs :::= List("--tt", mp3.title  )
              lameArgs :::= List[String](
                "-h", "-S", "--noreplaygain",  // high quality, silent, no AGC
                if (mp3.vbr) "--abr" else "-b", mp3.kbps.toString  // bit rate
              )
              blocking {
                val lame = new de.sciss.jump3r.Main
                val res  = lame.run(lameArgs.toArray)
                if (res != 0) throw new Exception(s"LAME mp3 encoder failed with code $res")
              }
              progress = 1.0
              checkAborted()

            } finally {
              fTmp.delete()
            }
        }

      } finally {
        if (afIn.isOpen) afIn.cleanUp()
        afIn.file.foreach(_.delete())
      }

      fileOut
    }
  }
}
abstract class ActionBounceTimeline[S <: Sys[S]](view: ViewHasWorkspace[S] with View.Editable[S],
                                              objH: stm.Source[S#Tx, Obj[S]])
  extends scala.swing.Action(ActionBounceTimeline.title) {

  import ActionBounceTimeline._

  private[this] var settings = QuerySettings[S]()

  protected def prepare(settings: QuerySettings[S]): QuerySettings[S] = settings

  protected type SpanPresets = ISeq[SpanPreset]

  protected def spanPresets(): SpanPresets = Nil

  final def apply(): Unit = {
    val setUpd          = prepare(settings)
    val presets         = spanPresets()
    val (_settings, ok) = query(view, setUpd, SpanSelection, presets)
    settings            = _settings
    if (ok) {
      for {
        f    <- _settings.file
        span <- _settings.span.nonEmptyOption
      }
        performGUI(view, _settings, objH :: Nil, f, span)
    }
  }
}