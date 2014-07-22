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
import de.sciss.synth.proc.{Timeline, Obj, AudioGraphemeElem, Artifact}
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.{event => evt}
import evt.Sys
import impl.{RecursionImpl => Impl}
import de.sciss.span.{Span, SpanLike}
import de.sciss.lucre.stm.Disposable
import de.sciss.serial.{Serializer, DataInput, Writable}
import de.sciss.synth.proc

object Recursion {
  type Channels = Vec[Range.Inclusive]
  type Update[S <: Sys[S]] = Unit

  final val typeID = 0x20000

  def apply[S <: Sys[S]](group: Timeline[S], span: Span, deployed: proc.Obj.T[S, AudioGraphemeElem],
                         gain: Gain, channels: Channels, transform: Option[Code.Obj[S]])
                        (implicit tx: S#Tx): Recursion[S] =
    Impl(group, span, deployed, gain, channels, transform)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Recursion[S] =
    Impl.serializer[S].read(in, access)

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Recursion[S]] = Impl.serializer

  // ---- element ----

  object Elem {
    def apply[S <: Sys[S]](peer: Recursion[S])(implicit tx: S#Tx): Recursion.Elem[S] = Impl.ElemImpl(peer)

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Recursion.Elem[S]] =
      Impl.ElemImpl.serializer
  }
  trait Elem[S <: Sys[S]] extends proc.Elem[S] {
    type Peer       = Recursion[S]
    type PeerUpdate = Recursion.Update[S]

    def mkCopy()(implicit tx: S#Tx): Elem[S]
  }

  object Obj {
    def unapply[S <: Sys[S]](obj: Obj[S]): Option[Recursion.Obj[S]] =
      if (obj.elem.isInstanceOf[Recursion.Elem[S]]) Some(obj.asInstanceOf[Recursion.Obj[S]])
      else None
  }
  type Obj[S <: Sys[S]] = proc.Obj.T[S, Recursion.Elem]
}
trait Recursion[S <: Sys[S]] extends Writable with Disposable[S#Tx] with evt.Publisher[S, Recursion.Update[S]] {
  import Recursion.Channels

  def group: Timeline[S]
  def span(implicit tx: S#Tx): Span
  def span_=(value: Span)(implicit tx: S#Tx): Unit
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
}