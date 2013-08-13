/*
 *  Gain.scala
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

package de.sciss.mellite

import de.sciss.serial.{Writable, DataOutput, DataInput, ImmutableSerializer}
import de.sciss.synth

object Gain {
  private final val COOKIE = 0x4761   // "Ga"

  def immediate (decibels: Float) = Gain(decibels, normalized = false)
  def normalized(decibels: Float) = Gain(decibels, normalized = true )

  implicit object Serializer extends ImmutableSerializer[Gain] {
    def write(v: Gain, out: DataOutput): Unit = v.write(out)
    def read(in: DataInput): Gain = Gain.read(in)
  }

  def read(in: DataInput): Gain = {
    val cookie      = in.readShort()
    require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
    val decibels      = in.readFloat()
    val normalized  = in.readByte() != 0
    Gain(decibels, normalized)
  }
}
final case class Gain(decibels: Float, normalized: Boolean) extends Writable {
  def linear: Float = {
    import synth._
    decibels.dbamp
  }

  def write(out: DataOutput): Unit = {
    out.writeShort(Gain.COOKIE)
    out.writeFloat(decibels)
    out.writeByte(if (normalized) 1 else 0)
  }
}