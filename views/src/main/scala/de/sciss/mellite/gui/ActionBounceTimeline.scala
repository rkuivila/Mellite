/*
 *  ActionBounceTimeline.scala
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

package de.sciss
package mellite
package gui

import java.io.{EOFException, File}
import java.text.ParseException
import javax.swing.{JFormattedTextField, SpinnerNumberModel, SwingUtilities}

import de.sciss.audiowidgets.{AxisFormat, TimelineModel}
import de.sciss.desktop.{Desktop, DialogSource, FileDialog, OptionPane, UndoManager, Window}
import de.sciss.file._
import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.expr.{Double => DoubleEx, Long => LongEx}
import de.sciss.lucre.geom.LongSquare
import de.sciss.lucre.stm
import de.sciss.lucre.swing._
import de.sciss.lucre.synth.{Buffer, Server, Synth, Sys}
import de.sciss.mellite.gui.edit.EditFolderInsertObj
import de.sciss.processor.impl.ProcessorImpl
import de.sciss.processor.{Processor, ProcessorLike}
import de.sciss.span.Span.SpanOrVoid
import de.sciss.span.{Span, SpanLike}
import de.sciss.swingplus.{ComboBox, Labeled, Spinner, SpinnerComboBox}
import de.sciss.synth.io.{AudioFile, AudioFileSpec, AudioFileType, SampleFormat}
import de.sciss.synth.proc.Implicits._
import de.sciss.synth.proc.{ArtifactLocationElem, AudioGraphemeElem, Bounce, Code, ExprImplicits, FolderElem, Grapheme, Obj, Timeline}
import de.sciss.synth.{SynthGraph, addToTail}

import scala.collection.immutable.{IndexedSeq => Vec}
import scala.concurrent.blocking
import scala.swing.Swing._
import scala.swing.event.{ButtonClicked, SelectionChanged}
import scala.swing.{Alignment, BoxPanel, Button, ButtonGroup, CheckBox, Component, Dialog, FlowPanel, GridPanel, Label, Orientation, ProgressBar, RadioButton, Swing, TextField}
import scala.util.Try
import scala.util.control.NonFatal

object ActionBounceTimeline {
  private val DEBUG = false

  final case class QuerySettings[S <: Sys[S]](
    file        : Option[File]          = None,
    spec        : AudioFileSpec         = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = 44100.0),
    gain        : Gain                  = Gain.normalized(-0.2f),
    span        : SpanOrVoid            = Span.Void,
    channels    : Vec[Range.Inclusive]  = Vector(0 to 0 /* 1 */),
    realtime    : Boolean               = false,
    fineControl : Boolean               = false,
    importFile  : Boolean               = false,
    location    : Option[stm.Source[S#Tx, ArtifactLocationElem.Obj[S]]] = None,
    transform   : Option[stm.Source[S#Tx, Code.Obj[S]]] = None
  ) {
    def prepare(group: stm.Source[S#Tx, Timeline.Obj[S]], f: File): PerformSettings[S] = {
      val server        = Server.Config()
      Mellite.applyAudioPrefs(server, useDevice = realtime, pickPort = realtime)
      if (fineControl) server.blockSize = 1
      specToServerConfig(f, spec, server)
      PerformSettings(
        realtime = realtime, group = group, server = server, gain = gain, span = span, channels = channels
      )
    }
  }

  final case class PerformSettings[S <: Sys[S]](
    realtime: Boolean,
    group: stm.Source[S#Tx, Timeline.Obj[S]],
    server: Server.Config,
    gain: Gain = Gain.normalized(-0.2f),
    span: SpanLike, channels: Vec[Range.Inclusive]
  )

  def specToServerConfig(file: File, spec: AudioFileSpec, config: Server.ConfigBuilder): Unit = {
    config.nrtOutputPath      = file.path
    config.nrtHeaderFormat    = spec.fileType
    config.nrtSampleFormat    = spec.sampleFormat
    config.sampleRate         = spec.sampleRate.toInt
    config.outputBusChannels  = spec.numChannels
    val numPrivate = Prefs.audioNumPrivate.getOrElse(Prefs.defaultAudioNumPrivate)
    config.audioBusChannels   = config.outputBusChannels + numPrivate
  }

  type CodeSource[S <: Sys[S]] = stm.Source[S#Tx, Code.Obj[S]]

  def findTransforms[S <: Sys[S]](document: Workspace[S])(implicit tx: S#Tx): Vec[Labeled[CodeSource[S]]] = {
    type Res = Vec[Labeled[CodeSource[S]]]
    def loop(xs: List[Obj[S]], res: Res): Res =
      xs match {
        case Code.Obj(objT) :: tail =>
          val res1 = objT.elem.peer.value match {
            case ft: Code.FileTransform => res :+ Labeled(tx.newHandle(objT))(objT.name)
            case _ => res
          }
          loop(tail, res1)
        case FolderElem.Obj(objT) :: tail =>
          val res1 = loop(objT.elem.peer.iterator.toList, res)
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

  def query[S <: Sys[S]](init: QuerySettings[S], document: Workspace[S], timelineModel: TimelineModel,
                         window: Option[Window])
                        (implicit cursor: stm.Cursor[S], undoManager: UndoManager) : (QuerySettings[S], Boolean) =
    query1(init, document, timelineModel, window, showSelection = SpanSelection, showTransform = true, showImport = true)

  def query1[S <: Sys[S]](init: QuerySettings[S], document: Workspace[S], timelineModel: TimelineModel,
                         window: Option[Window], showSelection: Selection, showTransform: Boolean,
                         showImport: Boolean)
                        (implicit cursor: stm.Cursor[S], undoManager: UndoManager) : (QuerySettings[S], Boolean) = {

    val ggFileType      = new ComboBox[AudioFileType](AudioFileType.writable)
    ggFileType.selection.item = init.spec.fileType // AudioFileType.AIFF
    val ggSampleFormat  = new ComboBox[SampleFormat](SampleFormat.fromInt16)
    desktop.Util.fixWidth(ggSampleFormat)
    // ggSampleFormat.items = fuck you scala no method here
    ggSampleFormat.selection.item = init.spec.sampleFormat
    val ggSampleRate    = new SpinnerComboBox(value0 = 44100.0, minimum = 1.0, maximum = Timeline.SampleRate,
      step = 100.0, items = Seq(44100.0, 48000.0, 88200.0, 96000.0))

    val ggPathText = new TextField(32)

    def setPathText(file: File): Unit =
      ggPathText.text = file.replaceExt(ggFileType.selection.item.extension).path

    ggFileType.listenTo(ggFileType.selection)
    ggFileType.reactions += {
      case SelectionChanged(_) =>
        val s = ggPathText.text
        if (!s.isEmpty) setPathText(new File(s))
    }

    init.file.foreach(f => ggPathText.text = f.path)
    val ggPathDialog    = Button("...") {
      val initT = ggPathText.text
      val init  = if (initT.isEmpty) None else Some(new File(initT))
      val dlg   = FileDialog.save(init = init, title = "Bounce Audio Output File")
      dlg.show(window).foreach(setPathText)
    }
    ggPathDialog.peer.putClientProperty("JButton.buttonType", "gradient")

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
    lazy val ggSpanUser  = new RadioButton(s"Current Selection $selectionText")
    lazy val ggSpanGroup = new ButtonGroup(ggSpanAll, ggSpanUser)

    lazy val mDuration   = new SpinnerNumberModel(tlSel.length / Timeline.SampleRate, 0.0, 10000.0, 0.1)

    var transformItemsCollected = false

    val ggRealtime = new CheckBox()
    ggRealtime.selected = init.realtime
    val ggFineControl = new CheckBox()
    ggFineControl.selected = init.fineControl

    def updateTransformEnabled(): Unit = {
      val enabled = ggImport.selected
      checkTransform.enabled = enabled
      ggTransform   .enabled = enabled && checkTransform.selected
      if (ggTransform.enabled && !transformItemsCollected) {
        val transform = cursor.step { implicit tx =>
          findTransforms(document)
        }
        transformItemsCollected =  true
        ggTransform.items = transform
        for (t <- init.transform; lb <- transform.find(_.value == t)) {
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
    if (showTransform) updateTransformEnabled()

    lazy val checkTransform = new CheckBox() {
      listenTo(this)
      reactions += {
        case ButtonClicked(_) => updateTransformEnabled()
      }
    }
    lazy val ggTransform = new ComboBox[Labeled[stm.Source[S#Tx, Obj.T[S, Code.Elem]]]](Nil) // lazy filling
    val pTransform  = new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(checkTransform, HStrut(4), ggTransform)
    }

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
    val pPath     = new FlowPanel(ggPathText, ggPathDialog)
    val pFormat   = new FlowPanel(ggFileType, ggSampleFormat, ggSampleRate, ggGainAmt, new Label("dB"), ggGainType)
    val pSpan     = new GridPanel(0, 2)
    pSpan.hGap    = 4
    pSpan.contents ++= Seq(new Label("Channels:", EmptyIcon, Trailing), ggChannels)
    if (showSelection == SpanSelection) {
      ggSpanGroup.select(if (init.span.isEmpty) ggSpanAll else ggSpanUser)
      ggSpanUser.enabled   = tlSel.nonEmpty
      pSpan.contents ++= Seq(new Label("Timeline Span:", EmptyIcon, Trailing), ggSpanAll,
                             HStrut(1),                                        ggSpanUser)
    } else if (showSelection == DurationSelection) {
      val ggDuration = new Spinner(mDuration)
      pSpan.contents ++= Seq(new Label("Duration [sec]:", EmptyIcon, Trailing), ggDuration)
    }
    pSpan.contents ++= Seq(
      HStrut(1), VStrut(32),
      new Label("Run in Real-Time:" , EmptyIcon, Trailing), ggRealtime,
      new Label("Fine Control Rate:", EmptyIcon, Trailing), ggFineControl
    )
    if (showImport) {
      ggImport.selected = init.importFile
      pSpan.contents ++= Seq(new Label("Import Output File Into Workspace:", EmptyIcon, Trailing), ggImport)
    }
    if (showTransform) {
      pSpan.contents ++= Seq(new Label("Apply Transformation:", EmptyIcon, Trailing), pTransform)
    }
    val box       = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(pPath, pFormat, pSpan)
    }

    val opt = OptionPane.confirmation(message = box, optionType = OptionPane.Options.OkCancel,
      messageType = OptionPane.Message.Plain)
    val title = "Bounce to Disk"
    opt.title = title
    val ok    = opt.show(window) == OptionPane.Result.Ok
    val file  = if (ggPathText.text == "") None else Some(new File(ggPathText.text))

    val channels: Vec[Range.Inclusive] = try {
      fmtRanges.stringToValue(ggChannelsJ.getText)
    } catch {
      case _: ParseException => init.channels
    }

    val importFile  = if (showImport) ggImport.selected else init.importFile
    val numChannels = channels.map(_.size).sum

    val spanOut = showSelection match {
      case SpanSelection      => if (ggSpanUser.selected) tlSel else Span.Void
      case DurationSelection  => Span(0L, (mDuration.getNumber.doubleValue() * Timeline.SampleRate + 0.5).toLong)
      case NoSelection        => init.span
    }

    var settings = QuerySettings(
      realtime    = ggRealtime   .selected,
      fineControl = ggFineControl.selected,
      file        = file,
      spec        = AudioFileSpec(ggFileType.selection.item, ggSampleFormat.selection.item,
        numChannels = numChannels, sampleRate = ggSampleRate.value),
      gain        = Gain(gainModel.getNumber.floatValue(), if (ggGainType.selection.index == 0) true else false),
      span        = spanOut,
      channels    = channels,
      importFile  = importFile,
      location    = init.location,
      transform   = if (checkTransform.selected) Option(ggTransform.selection.item).map(_.value) else None
    )

    file match {
      case Some(f) if importFile =>
        init.location match {
          case Some(source) if cursor.step { implicit tx =>
              val parent = source().elem.peer.directory
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
        ) == Dialog.Result.Ok
        (settings, ok1)

      case None if ok =>
        query(settings, document, timelineModel, window)
      case _ =>
        (settings, ok)
    }
  }

  //  final case class QuerySettings[S <: Sys[S]](
  //    file: Option[File] = None,
  //    spec: AudioFileSpec = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = 44100.0),
  //    gainAmount: Double = -0.2, gainType: Gain = Normalized,
  //    span: SpanOrVoid = Span.Void, channels: Vec[Range.Inclusive] = Vector(0 to 1),
  //    importFile: Boolean = true, location: Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None
  //  )

  def performGUI[S <: Sys[S]](document: Workspace[S],
                              settings: QuerySettings[S],
                              group: stm.Source[S#Tx, Timeline.Obj[S]], file: File,
                              window: Option[Window] = None)
                             (implicit cursor: stm.Cursor[S], compiler: Code.Compiler): Unit = {

    val hasTransform= settings.importFile && settings.transform.isDefined
    val bounceFile  = if (hasTransform) {
      File.createTempFile("bounce", s".${settings.spec.fileType.extension}")
    } else {
      file
    }
    val pSet        = settings.prepare(group, bounceFile)
    var process: ProcessorLike[Any, Any] = perform(document, pSet)

    var processCompleted = false

    val ggProgress  = new ProgressBar()
    lazy val ggCancel = Button("Abort") {
      process.abort()
      // currently process doesn't seem to abort under certain errors
      // (e.g. buffer allocator exhausted). XXX TODO
      val run = new Runnable { def run() = { Thread.sleep(1000); defer(fDispose()) }}
      new Thread(run).start()
    }
    ggCancel.focusable = false
    lazy val op     = OptionPane(message = ggProgress, messageType = OptionPane.Message.Plain, entries = Seq(ggCancel))
    lazy val title  = s"Bouncing to ${file.name} ..."
    op.title  = title

    def fDispose(): Unit = {
      val w = SwingUtilities.getWindowAncestor(op.peer); if (w != null) w.dispose()
      processCompleted = true
    }
    val progDiv = if (hasTransform) 2 else 1
    process.addListener {
      case prog @ Processor.Progress(_, _) => defer(ggProgress.value = prog.toInt / progDiv)
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
      if (DEBUG) println(s"bounceDone(). hasTransform? $hasTransform")
      if (hasTransform) {
        val ftOpt = cursor.step { implicit tx =>
          settings.transform.flatMap(_.apply().elem.peer.value match {
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
                case prog @ Processor.Progress(_, _) => defer(ggProgress.value = prog.toInt / progDiv + 50)
              }
              codeProc.onSuccess { case _ => allDone() }
              codeProc.onFailure(onFailure)
            }))
          case _ =>
            println("WARNING: Code does not denote a file transform")
            defer(fDispose())
            Desktop.revealFile(file)
        }

      } else {
        allDone()
      }
    }

    def allDone(): Unit = {
      if (DEBUG) println("allDone")
      defer(fDispose())
      (settings.importFile, settings.location) match {
        case (true, Some(locSource)) =>
          val elemName  = file.base
          val spec      = AudioFile.readSpec(file)
          cursor.step { implicit tx =>
            val loc       = locSource()
            loc.elem.peer.modifiableOption.foreach { locM =>
              val imp = ExprImplicits[S]
              import imp._
              // val fileR     = Artifact.relativize(locM.directory, file)
              // val artifact  = locM.add(file)
              // val depArtif  = Artifact.Modifiable(artifact)
              val depArtif  = locM.add(file)
              val depOffset = LongEx  .newVar(0L)
              val depGain   = DoubleEx.newVar(1.0)
              val deployed  = Grapheme.Expr.Audio(depArtif, spec, depOffset, depGain)
              val depElem   = AudioGraphemeElem(deployed)
              val depObj    = Obj(depElem)
              depObj.name = file.base
              val transformOpt = settings.transform.map(_.apply())
              val recursion = Recursion(group(), settings.span, depObj, settings.gain, settings.channels, transformOpt)
              val recElem   = Recursion.Elem(recursion)
              val recObj    = Obj(recElem)
              recObj.name = elemName
              document.rootH().addLast(depObj)
              document.rootH().addLast(recObj)
            }
          }

        case _ =>
          Desktop.revealFile(file)
      }
    }

    process.onSuccess { case _ => bounceDone() }
    process.onFailure(onFailure)

    desktop.Util.delay(500) {
      if (!processCompleted) op.show(window)
    }
  }

  def perform[S <: Sys[S]](document: Workspace[S], settings: PerformSettings[S])
                          (implicit cursor: stm.Cursor[S]): Processor[File] = {
    import document.inMemoryBridge
    implicit val workspace = document

    // for real-time, we generally have to overshoot because in SC 3.6, DiskOut's
    // buffer is not flushed after synth is stopped.
    val realtime      = settings.realtime
    val normalized    = settings.gain.normalized
    val needsTemp     = realtime || normalized
    val numChannels   = settings.server.outputBusChannels

    val span = settings.span match {
      case sp: Span => sp
      case _ =>
        cursor.step { implicit tx =>
          val tl = settings.group().elem.peer
          val start = settings.span match {
            case hs: Span.HasStart => hs.start
            case _ =>
              // XXX TODO -- should be exposed in BiGroup!
              val MAX_SQUARE  = LongSquare(0, 0, 0x2000000000000000L)
              val MIN_COORD   = MAX_SQUARE.left
              tl.nearestEventAfter(MIN_COORD + 1).getOrElse(0L)
          }
          val stop = settings.span match {
            case hs: Span.HasStop => hs.stop
            case _ =>
              // XXX TODO -- should be exposed in BiGroup!
              val MAX_SQUARE  = LongSquare(0, 0, 0x2000000000000000L)
              val MAX_COORD   = MAX_SQUARE.right
              tl.nearestEventBefore(MAX_COORD - 1).getOrElse(start)
          }
          Span(start, stop)
        }
    }

    val fileOut       = file(settings.server.nrtOutputPath)
    val fileType      = settings.server.nrtHeaderFormat
    val sampleFormat  = settings.server.nrtSampleFormat
    val sampleRate    = settings.server.sampleRate
    val fileFrames0   = (span.length * sampleRate / Timeline.SampleRate + 0.5).toLong
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
    bnc.group     = settings1.group :: Nil
    bnc.realtime  = realtime
    bnc.server.read(settings1.server)

    val bncGainFactor = if (settings1.gain.normalized) 1f else settings1.gain.linear
    val inChans       = settings1.channels.flatten
    val numInChans    = if (inChans.isEmpty) 0 else inChans.max + 1
    assert(numInChans >= numChannels)

    val span1 = if (!realtime) span else {
      val bufDur    = Buffer.defaultRecBufferSize.toDouble / bnc.server.sampleRate
      // apart from DiskOut buffer, add a bit of head-room (100ms) to account for jitter
      val bufFrames = ((bufDur + 0.1) * Timeline.SampleRate + 0.5).toLong
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
      Synth.play(graph)(s.defaultGroup, addAction = addToTail)
    }

    val bProcess  = bounce.apply(bnc)
    // bProcess.addListener {
    //   case u => println(s"UPDATE: $u")
    // }
    bProcess.start()
    val process   = if (!needsTemp) bProcess else {
      val nProcess = new Normalizer(bounce = bProcess,
        fileOut = fileOut, fileType = fileType, sampleFormat = sampleFormat,
        gain = if (normalized) settings1.gain else Gain.immediate(0f), numFrames = fileFrames)
      nProcess.start()
      nProcess
    }
    process
  }

  // XXX TODO --- could use filtered console output via Poll to
  // measure max gain already during bounce
  private final class Normalizer[S <: Sys[S]](bounce: Processor[File],
                                              fileOut: File, fileType: AudioFileType, sampleFormat: SampleFormat,
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

      val afIn    = AudioFile.openRead(fileIn)

      import numbers.Implicits._
      try {
        val bufSz       = 8192
        val buf         = afIn.buffer(bufSz)
        val numFrames0  = math.min(afIn.numFrames, numFrames) // whatever...
        var rem         = numFrames0
//        if (rem >= afIn.numFrames)
//          throw new EOFException(s"Bounced file is too short (${afIn.numFrames} -- expected at least $rem)")
        if (afIn.numFrames == 0)
          throw new EOFException("Bounced file is empty")

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
        val afOut = AudioFile.openWrite(fileOut,
          afIn.spec.copy(fileType = fileType, sampleFormat = sampleFormat, byteOrder = None))
        try {
          rem = numFrames0
          while (rem > 0) {
            val chunk = math.min(bufSz, rem).toInt
            afIn.read(buf, 0, chunk)
            if (mul != 1) {
              var ch = 0; while (ch < buf.length) {
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
            progress = (rem.toDouble / numFrames0).linlin(0, 1, 1.0, 0.9)
            checkAborted()
          }
          afOut.close()
          afIn .close()
        } finally {
          if (afOut.isOpen) afOut.cleanUp()
        }

      } finally {
        if (afIn.isOpen) afIn.cleanUp()
      }

      fileOut
    }
  }
}