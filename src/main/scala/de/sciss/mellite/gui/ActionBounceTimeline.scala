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
import javax.swing.{JSpinner, SpinnerNumberModel}
import de.sciss.span.Span
import Swing._
import de.sciss.audiowidgets.AxisFormat
import de.sciss.mellite.gui.impl.TimelineProcView
import scala.annotation.switch
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.mellite.Element.ArtifactLocation

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
    span: SpanOrVoid = Span.Void, channels: IIdxSeq[Int] = Vector(0, 1),
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
    init.file.foreach(f => ggPathText.text = f.path)
    val ggPathDialog    = Button("...") {
      val dlg = FileDialog.save(init = Some(new File(ggPathText.text)), title = "Bounce Audio Output File")
      dlg.show(parent).foreach { file =>
        ggPathText.text = file.replaceExtension(ggFileType.selection.item.extension).path
      }
    }
    ggPathDialog.peer.putClientProperty("JButton.buttonType", "gradient")

    val gainModel   = new SpinnerNumberModel(init.gainAmount, -160.0, 160.0, 0.1)
    val ggGainAmtJ  = new JSpinner(gainModel)
    println(ggGainAmtJ.getPreferredSize)
    val ggGainAmt   = Component.wrap(ggGainAmtJ)

    val ggGainType  = new ComboBox[GainType](Seq(Normalized, Immediate))
    ggGainType.selection.item = init.gainType

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

    //    val box = new GroupPanel {
    //      val lbName  = new Label( "Name:", EmptyIcon, Alignment.Right)
    //      val lbValue = new Label("Value:", EmptyIcon, Alignment.Right)
    //      theHorizontalLayout is Sequential(Parallel(Trailing)(lbName, lbValue), Parallel(ggName, value))
    //      theVerticalLayout   is Sequential(Parallel(Baseline)(lbName, ggName ), Parallel(Baseline)(lbValue, value))
    //    }

    val pPath     = new FlowPanel(ggPathText, ggPathDialog)
    val pFormat   = new FlowPanel(ggFileType, ggSampleFormat, ggGainAmt, ggGainType)
    val pSpan     = new GridPanel(2, 2) {
      contents ++= Seq(new Label("Timeline Span:", Swing.EmptyIcon, Alignment.Right), ggSpanAll,
                       new Label(""),                                                 ggSpanUser)
    }

    val box       = new BoxPanel(Orientation.Vertical) {
      contents ++= Seq(pPath, pFormat, pSpan, VStrut(32), ggImport)
    }

    val opt = OptionPane.confirmation(message = box, optionType = OptionPane.Options.OkCancel,
      messageType = OptionPane.Message.Plain)
    opt.title = "Bounce Timeline to Disk"
    val ok = opt.show(parent) == OptionPane.Result.Ok

    val channels: IIdxSeq[Int] = ???
    val location: Option[stm.Source[S#Tx, ArtifactLocation[S]]] = ???

    val settings = Settings(
      file        = if (ggPathText.text == "") None else Some(new File(ggPathText.text)),
      spec        = AudioFileSpec(ggFileType.selection.item, ggSampleFormat.selection.item,
        numChannels = channels.size, sampleRate = timelineModel.sampleRate),
      gainAmount  = gainModel.getNumber.doubleValue(),
      gainType    = ggGainType.selection.item,
      span        = if (ggSpanUser.selected) tlSel else Span.Void,
      channels    = channels,
      importFile  = ggImport.selected,
      location    = location
    )

    (settings, ok)
  }

  def perform[S <: Sys[S]](groupH: stm.Source[S#Tx, proc.ProcGroup[S]], span: Span,
                           selection: Option[Iterable[TimelineProcView[S]]], parent: Option[Window]) {

    // val bounce = Bounce[S, I]
    ???
  }
}