/*
 *  Recursion.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite

import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Obj, AudioGraphemeElem, Artifact, ProcGroup}
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.event.EventLike
import impl.{RecursionImpl => Impl}
import de.sciss.span.SpanLike
import de.sciss.lucre.stm.Disposable
import de.sciss.serial.{DataInput, Writable}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc

object Recursion {
  type Channels = Vec[Range.Inclusive]
  type Update[S <: Sys[S]] = Unit

  def apply[S <: Sys[S]](group: ProcGroup[S], span: SpanOrVoid, deployed: Obj.T[S, AudioGraphemeElem],
                         gain: Gain, channels: Channels, transform: Option[Obj.T[S, Code.Elem]])
                        (implicit tx: S#Tx): Recursion[S] =
    Impl(group, span, deployed, gain, channels, transform)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Recursion[S] =
    Impl.serializer.read(in, access)

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Recursion[S]] = Impl.serializer

  // ---- element ----
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer = Recursion[S]

    def mkCopy()(implicit tx: S#Tx): Elem[S]
  }
}
trait Recursion[S <: Sys[S]] extends Writable with Disposable[S#Tx] {
  import Recursion.Channels

  def group: ProcGroup[S]
  def span(implicit tx: S#Tx): SpanLike
  def span_=(value: SpanLike)(implicit tx: S#Tx): Unit
  def deployed: Obj.T[S, AudioGraphemeElem] //  Grapheme.Elem.Audio[S]
  def product: Artifact[S]
  def productSpec: AudioFileSpec
  def gain(implicit tx: S#Tx): Gain
  def gain_=(value: Gain)(implicit tx: S#Tx): Unit
  def channels(implicit tx: S#Tx): Channels
  def channels_=(value: Vec[Range.Inclusive])(implicit tx: S#Tx): Unit

  def transform: Option[Obj.T[S, Code.Elem]]
  // def transform_=(value: Option[Element.Code[S]])(implicit tx: S#Tx): Unit

  /** Moves the product to deployed position. */
  def iterate()(implicit tx: S#Tx): Unit

  def changed: EventLike[S, Recursion.Update[S]]
}