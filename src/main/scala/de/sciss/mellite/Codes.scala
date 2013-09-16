/*
 *  Codes.scala
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

import de.sciss.lucre.event.{Targets, Sys}
import de.sciss.serial.{DataOutput, DataInput}
import de.sciss.lucre.synth.expr.BiTypeImpl

object Codes extends BiTypeImpl[Code] {
  final val typeID = 0x20001

  def readValue(in: DataInput): Code = Code.read(in)
  def writeValue(value: Code, out: DataOutput): Unit = value.write(out)

  protected def readTuple[S <: Sys[S]](cookie: Int, in: DataInput, access: S#Acc, targets: Targets[S])
                                      (implicit tx: S#Tx): Codes.ReprNode[S] = {
    sys.error(s"No tuple operations defind for Code ($cookie)")
  }
}