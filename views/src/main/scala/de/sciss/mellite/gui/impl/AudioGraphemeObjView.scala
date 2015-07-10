/*
 *  AudioGraphemeObjView.scala
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

import de.sciss.audiowidgets.AxisFormat
import de.sciss.desktop
import de.sciss.desktop.FileDialog
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.swing.{deferTx, Window}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.model.Change
import de.sciss.synth.io.{AudioFile, AudioFileSpec, SampleFormat}
import de.sciss.synth.proc.impl.ElemImpl
import de.sciss.synth.proc.{ArtifactLocationElem, AudioGraphemeElem, Grapheme, Obj}

import scala.swing.{Component, Label}
import scala.util.Try

object AudioGraphemeObjView extends ListObjView.Factory {
  type E[S <: evt.Sys[S]] = AudioGraphemeElem[S]
  val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Music)
  val prefix    = "AudioGrapheme"
  def humanName = "Audio File"
  def typeID    = ElemImpl.AudioGrapheme.typeID
  def hasMakeDialog = true

  def category = ObjView.categResources

  def mkListView[S <: Sys[S]](obj: Obj.T[S, AudioGraphemeElem])
                             (implicit tx: S#Tx): AudioGraphemeObjView[S] with ListObjView[S] = {
    val value = obj.elem.peer.value
    new Impl(tx.newHandle(obj), value).initAttrs(obj)
  }

  final case class Config1[S <: evt.Sys[S]](file: File, spec: AudioFileSpec,
                                            location: Either[stm.Source[S#Tx, ArtifactLocationElem.Obj[S]], (String, File)])
  type Config[S <: evt.Sys[S]] = List[Config1[S]]

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] = {
    val dlg = FileDialog.open(init = None /* locViews.headOption.map(_.directory) */, title = "Add Audio Files")
    dlg.setFilter(f => Try(AudioFile.identify(f).isDefined).getOrElse(false))
    dlg.multiple = true
    val ok = dlg.show(window).isDefined

    val list = if (!ok) Nil else dlg.files.flatMap { f =>
      ActionArtifactLocation.query[S](workspace.rootH, file = f, window = window).map { location =>
        val spec = AudioFile.readSpec(f)
        Config1(file = f, spec = spec, location = location)
      }
    }
    if (list.isEmpty) None else Some(list)
  }

  def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]] = config.flatMap { cfg =>
    val (list0: List[Obj[S]], loc /* : ArtifactLocationElem.Obj[S] */) = cfg.location match {
      case Left(source) => (Nil, source())
      case Right((name, directory)) =>
        val objLoc  = ActionArtifactLocation.create(name = name, directory = directory)
        (objLoc :: Nil, objLoc)
    }
    loc.elem.peer.modifiableOption.fold[List[Obj[S]]](list0) { locM =>
      val audioObj = ObjectActions.mkAudioFile(locM, cfg.file, cfg.spec)
      audioObj :: list0
    }
  }

  private val timeFmt = AxisFormat.Time(hours = false, millis = true)

  final class Impl[S <: Sys[S]](protected val objH: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]],
                                var value: Grapheme.Value.Audio)
    extends AudioGraphemeObjView[S]
    with ListObjView /* .AudioGrapheme */[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.NonEditable[S] {

    override def obj(implicit tx: S#Tx) = objH()

    type E[~ <: evt.Sys[~]] = AudioGraphemeElem[~]

    def factory = AudioGraphemeObjView

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = update match {
      case Change(_, now: Grapheme.Value.Audio) =>
        deferTx { value = now }
        true
      case _ => false
    }

    def isViewable = true

    def openView(parent: Option[Window[S]])
                (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]] = {
      val frame = AudioFileFrame(obj)
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
trait AudioGraphemeObjView[S <: evt.Sys[S]] extends ObjView[S] {
  override def obj(implicit tx: S#Tx): AudioGraphemeElem.Obj[S]
}