/*
 *  Recursion.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss
package mellite

import de.sciss.synth.io.AudioFileSpec
import de.sciss.synth.proc.{Sys, Artifact, ProcGroup}
import de.sciss.span.Span.SpanOrVoid
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.event.EventLike
import impl.{RecursionImpl => Impl}
import de.sciss.span.SpanLike
import de.sciss.lucre.stm.Disposable
import de.sciss.serial.{DataInput, Writable}

object Recursion {
  type Channels = Vec[Range.Inclusive]
  type Update[S <: Sys[S]] = Unit

  def apply[S <: Sys[S]](group: ProcGroup[S], span: SpanOrVoid, deployed: Element.AudioGrapheme[S],
                         gain: Gain, channels: Channels, transform: Option[Element.Code[S]])
                        (implicit tx: S#Tx): Recursion[S] =
    Impl(group, span, deployed, gain, channels, transform)

  def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Recursion[S] =
    Impl.serializer.read(in, access)

  implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Recursion[S]] = Impl.serializer
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
  def channels_=(value: Vec[Range.Inclusive])(implicit tx: S#Tx): Unit

  def transform: Option[Element.Code[S]]
  // def transform_=(value: Option[Element.Code[S]])(implicit tx: S#Tx): Unit

  /** Moves the product to deployed position. */
  def iterate()(implicit tx: S#Tx): Unit

  def changed: EventLike[S, Recursion.Update[S]]
}