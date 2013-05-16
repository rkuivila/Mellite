package de.sciss
package mellite

import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Grapheme, Sys, Artifact, ProcGroup}
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => IIdxSeq}
import de.sciss.lucre.event.EventLike
import impl.{RecursionImpl => Impl}
import de.sciss.span.SpanLike
import de.sciss.lucre.stm.Disposable
import de.sciss.serial.{DataInput, Writable}

object Recursion {
  type Channels = IIdxSeq[Range.Inclusive]
  type Update[S <: Sys[S]] = Unit

  def apply[S <: Sys[S]](group: ProcGroup[S], span: SpanOrVoid, deployed: Element.AudioGrapheme[S],
                         gain: Gain, channels: Channels)(implicit tx: S#Tx): Recursion[S] =
    Impl(group, span, deployed, gain, channels)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Recursion[S] =
    Impl.serializer.read(in, access)
}
trait Recursion[S <: Sys[S]] extends Writable with Disposable[S#Tx] {
  import Recursion.Channels

  def group: ProcGroup[S]
  def span(implicit tx: S#Tx): SpanLike
  def span_=(value: SpanLike)(implicit tx: S#Tx): Unit
  def deployed: Element.AudioGrapheme[S] //  Grapheme.Elem.Audio[S]
  def product: Artifact[S]
  def productSpec: AudioFileSpec
  def gain(implicit tx: S#Tx): Gain
  def gain_=(value: Gain)(implicit tx: S#Tx): Unit
  def channels(implicit tx: S#Tx): Channels
  def channels_=(value: IIdxSeq[Range.Inclusive])(implicit tx: S#Tx): Unit

  /** Moves the product to deployed position. */
  def iterate()(implicit tx: S#Tx): Unit

  def changed: EventLike[S, Recursion.Update[S], Recursion[S]]
}