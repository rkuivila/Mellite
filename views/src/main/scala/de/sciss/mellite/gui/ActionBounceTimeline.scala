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

import de.sciss.lucre.artifact.Artifact
import de.sciss.lucre.stm
import de.sciss.mellite.gui.edit.EditFolderInsertObj
import de.sciss.synth.{ugen, SynthGraph, addToTail, proc}
import de.sciss.synth.proc.{ArtifactLocationElem, Code, Timeline, AudioGraphemeElem, Obj, ExprImplicits, FolderElem, Grapheme, Bounce}
import de.sciss.desktop.{UndoManager, Desktop, DialogSource, OptionPane, FileDialog, Window}
import scala.swing.{Dialog, ProgressBar, Swing, Alignment, Label, GridPanel, Orientation, BoxPanel, FlowPanel, ButtonGroup, RadioButton, CheckBox, Component, Button, TextField}
import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat, AudioFileType}
import java.io.File
import javax.swing.{SwingUtilities, JFormattedTextField, SpinnerNumberModel}
import de.sciss.span.{SpanLike, Span}
import Swing._
import de.sciss.audiowidgets.{TimelineModel, AxisFormat}
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => Vec}
import scala.util.control.NonFatal
import java.text.ParseException
import scala.swing.event.{ButtonClicked, SelectionChanged}
import scala.util.{Failure, Try}
import de.sciss.processor.{ProcessorLike, Processor}
import de.sciss.file._
import de.sciss.lucre.swing._
import de.sciss.swingplus.{SpinnerComboBox, ComboBox, Spinner, Labeled}
import de.sciss.lucre.synth.{Server, Synth, Sys}
import de.sciss.lucre.expr.{Long => LongEx, Double => DoubleEx}
import proc.Implicits._

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
    gain: Gain          = Gain.normalized(-0.2f),
    span: SpanOrVoid    = Span.Void,
    channels: Vec[Range.Inclusive] = Vector(0 to 0 /* 1 */),
    importFile: Boolean = false,
    location:  Option[stm.Source[S#Tx, ArtifactLocationElem.Obj[S]]] = None,
    transform: Option[stm.Source[S#Tx, Code.Obj[S]]] = None
  ) {
    def prepare(group: stm.Source[S#Tx, Timeline.Obj[S]], f: File): PerformSettings[S] = {
      val server = Server.Config()
      specToServerConfig(f, spec, server)
      PerformSettings(
        group = group, server = server, gain = gain, span = span, channels = channels
      )
    }
  }

  final case class PerformSettings[S <: Sys[S]](
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

    import Swing.EmptyIcon
    import Alignment.Trailing
    val pPath     = new FlowPanel(ggPathText, ggPathDialog)
    val pFormat   = new FlowPanel(ggFileType, ggSampleFormat, ggSampleRate, ggGainAmt, new Label("dB"), ggGainType)
    val pSpan     = new GridPanel(0, 2)
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
    pSpan.contents ++= Seq(HStrut(1), VStrut(32))
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

              case _              => return (settings, false)
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

    val onFailure: PartialFunction[Any, Unit] = {
      case Failure(Processor.Aborted()) =>
        defer(fDispose())
      case Failure(e) =>
        defer {
          fDispose()
          DialogSource.Exception(e -> title).show(window)
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
    val bounce  = Bounce[S, document.I]
    val bnc     = Bounce.Config[S]
    bnc.group   = settings.group :: Nil
    bnc.server.read(settings.server)
    val spanL   = settings.span
    val span    = spanL match {
      case sp: Span => sp
      case _ =>
        cursor.step { implicit tx =>
          val tl = settings.group().elem.peer
          val start = spanL match {
            case hs: Span.HasStart => hs.start
            case _ => tl.nearestEventAfter(Long.MinValue + 1).getOrElse(0L)
          }
          val stop = spanL match {
            case hs: Span.HasStop => hs.stop
            case _ => tl.nearestEventBefore(Long.MaxValue - 1).getOrElse(start)
          }
          Span(start, stop)
        }
    }
    bnc.span    = span
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