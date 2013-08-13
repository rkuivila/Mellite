/*
 *  Transform.scala
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

import java.io.File
import scala.concurrent.Future
import de.sciss.processor.Processor
import de.sciss.serial.{DataOutput, DataInput, ImmutableSerializer}
import scala.annotation.switch

object Transform {
  private final val COOKIE  = 0x66726D00  // "frm\0"

  case object Unmodified extends Transform

  final case class Coded(source: Code.FileTransform) extends Transform {
    def perform(in: File, out: File, procHandler: Processor[Any, _] => Unit = _ => ()): Future[Unit] =
      source.execute((in, out, procHandler))
  }

  implicit object Serializer extends ImmutableSerializer[Transform] {
    def write(v: Transform, out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      v match { // "match may not be exhaustive" - not true, scalac bug
        case Unmodified   => out.writeByte(0)
        case Coded(source) =>
          out.writeByte(1)
          Code.serializer.write(source, out)
        case _ => sys.error("WHAT THE HECK SCALA")
      }
    }

    def read(in: DataInput): Transform = {
      val cookie = in.readInt()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (requires $COOKIE)")
      (in.readByte(): @switch) match {
        case 0 => Unmodified
        case 1 =>
          val source = Code.serializer.read(in) match {
            case ft: Code.FileTransform => ft
            case other => sys.error(s"Expected file transform code, but found $other")
          }
          Coded(source)
      }
    }
  }
}
sealed trait Transform