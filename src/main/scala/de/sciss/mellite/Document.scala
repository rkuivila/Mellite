/*
 *  Document.scala
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
import de.sciss.lucre.{expr, event => evt, bitemp, stm}
import expr.LinkedList
import bitemp.BiGroup
import de.sciss.synth.proc
import impl.{DocumentImpl => Impl}
import de.sciss.synth.proc.{AuralSystem, Proc, Sys}
import de.sciss.synth.expr.SpanLikes
import de.sciss.serial.Serializer

object Document {
  type Group       [S <: Sys[S]] = BiGroup.Modifiable   [S, Proc[S], Proc.Update[S]]
  type GroupUpdate [S <: Sys[S]] = BiGroup.Update       [S, Proc[S], Proc.Update[S]]

  type Groups      [S <: Sys[S]] = LinkedList.Modifiable[S, Group[S], GroupUpdate[S]]
  type GroupsUpdate[S <: Sys[S]] = LinkedList.Update    [S, Group[S], GroupUpdate[S]]

  type Transport   [S <: Sys[S]] = proc.ProcTransport[S]
  type Transports  [S <: Sys[S]] = LinkedList.Modifiable[S, Transport[S], Unit] // Transport.Update[ S, Proc[ S ]]]

  def read (dir: File): ConfluentDocument = Impl.read (dir)
  def empty(dir: File): ConfluentDocument = Impl.empty(dir)

  object Serializers {
    implicit def group[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Group[S]] with evt.Reader[S, Group[S]] = {
      implicit val spanType = SpanLikes
      BiGroup.Modifiable.serializer[S, Proc[S], Proc.Update[S]](_.changed)
    }
  }
}

sealed trait Document[S <: Sys[S]] {
  import Document.{Group => _, _}

  def system: S
  // implicit def cursor: Cursor[S]
  // def aural: AuralSystem[S]
  def folder: File
  // def cursors: Cursors[S, S#D]

  // def masterCursor: stm.Cursor[S]

  type I <: evt.Sys[I]
  implicit def inMemory: S#Tx => I#Tx

  def elements(implicit tx: S#Tx): Folder[S]

  implicit def systemType: reflect.runtime.universe.TypeTag[S]
}

trait ConfluentDocument extends Document[proc.Confluent] {
  type S = proc.Confluent
  def cursors: Cursors[S, S#D]
}

trait EphemeralDocument extends Document[proc.Durable] {
  type S = proc.Durable
  def cursor: stm.Cursor[S] = system
}