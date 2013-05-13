package de.sciss
package mellite
package gui

import de.sciss.lucre.stm
import de.sciss.synth.proc
import de.sciss.synth.proc.{Bounce, Sys}
import de.sciss.desktop.{OptionPane, FileDialog, Window}
import scala.swing.{Swing, Alignment, Label, GridPanel, Orientation, BoxPanel, FlowPanel, ButtonGroup, RadioButton, CheckBox, Component, ComboBox, Button, TextField}
import de.sciss.synth.io.{AudioFileSpec, SampleFormat, AudioFileType}
import java.io.File
import javax.swing.{JFormattedTextField, JSpinner, SpinnerNumberModel}
import de.sciss.span.Span
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

object ActionBounceTimeline {
  object GainType {
    def apply(id: Int): GainType = (id: @switch) match {
      case Normalized.id  => Normalized
      case Immediate .id  => Immediate
    }
  }
  sealed trait GainType { def id: Int }
  case object Normalized extends GainType { final val id = 0 }
  case object Immediate  extends GainType { final val id = 1 }

  final case class Settings[S <: Sys[S]](
    file: Option[File] = None,
    spec: AudioFileSpec = AudioFileSpec(AudioFileType.AIFF, SampleFormat.Int24, numChannels = 2, sampleRate = 44100.0),
    gainAmount: Double = -0.2, gainType: GainType = Normalized,
    span: SpanOrVoid = Span.Void, channels: IIdxSeq[Range.Inclusive] = Vector(0 to 1),
    importFile: Boolean = true, location: Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None
  )

  def query[S <: Sys[S]](init: Settings[S], document: Document[S], timelineModel: TimelineModel,
                         parent: Option[Window])
                        (implicit cursor: stm.Cursor[S]) : (Settings[S], Boolean) = {

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
      dlg.show(parent).foreach(setPathText)
    }
    ggPathDialog.peer.putClientProperty("JButton.buttonType", "gradient")

    val gainModel   = new SpinnerNumberModel(init.gainAmount, -160.0, 160.0, 0.1)
    val ggGainAmtJ  = new JSpinner(gainModel)
    // println(ggGainAmtJ.getPreferredSize)
    val ggGainAmt   = Component.wrap(ggGainAmtJ)

    val ggGainType  = new ComboBox[GainType](Seq(Normalized, Immediate))
    ggGainType.selection.item = init.gainType
    ggGainType.listenTo(ggGainType.selection)
    ggGainType.reactions += {
      case SelectionChanged(_) =>
        ggGainType.selection.item match {
          case Normalized => gainModel.setValue(-0.2)
          case Immediate  => gainModel.setValue( 0.0)
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
    val ok    = opt.show(parent) == OptionPane.Result.Ok
    val file  = if (ggPathText.text == "") None else Some(new File(ggPathText.text))

    val channels: IIdxSeq[Range.Inclusive] = try {
      fmtRanges.stringToValue(ggChannelsJ.getText)
    } catch {
      case _: ParseException => init.channels
    }
    val location: Option[stm.Source[S#Tx, ArtifactLocation[S]]] = None // ???

    val settings = Settings(
      file        = file,
      spec        = AudioFileSpec(ggFileType.selection.item, ggSampleFormat.selection.item,
        numChannels = channels.size, sampleRate = timelineModel.sampleRate),
      gainAmount  = gainModel.getNumber.doubleValue(),
      gainType    = ggGainType.selection.item,
      span        = if (ggSpanUser.selected) tlSel else Span.Void,
      channels    = channels,
      importFile  = ggImport.selected,
      location    = location
    )

    if (ok && file.isEmpty) return query(settings, document, timelineModel, parent)

    (settings, ok)
  }

  def perform[S <: Sys[S]](groupH: stm.Source[S#Tx, proc.ProcGroup[S]], span: Span,
                           selection: Option[Iterable[TimelineProcView[S]]], parent: Option[Window]) {

    // val bounce = Bounce[S, I]
    ???
  }
}