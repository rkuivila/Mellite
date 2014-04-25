/*
 *  Document.scala
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

package de.sciss.mellite

import java.io.File
import de.sciss.lucre.{expr, event => evt, bitemp, stm}
import bitemp.BiGroup
import de.sciss.synth.proc
import impl.{DocumentImpl => Impl}
import de.sciss.synth.proc.{Obj, Elem, Folder, Proc}
import de.sciss.serial.Serializer
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.{Sys => SSys}

object Document {
  type Group       [S <: Sys[S]] = BiGroup.Modifiable   [S, Proc[S], Proc.Update[S]]
  type GroupUpdate [S <: Sys[S]] = BiGroup.Update       [S, Proc[S], Proc.Update[S]]

  type Groups      [S <: Sys[S]] = expr.List.Modifiable[S, Group[S], GroupUpdate[S]]
  type GroupsUpdate[S <: Sys[S]] = expr.List.Update    [S, Group[S], GroupUpdate[S]]

  type Transport   [S <: Sys[S]] = proc.ProcTransport[S]
  type Transports  [S <: Sys[S]] = expr.List.Modifiable[S, Transport[S], Unit] // Transport.Update[ S, Proc[ S ]]]

  def read (dir: File): ConfluentDocument = Impl.read (dir)
  def empty(dir: File): ConfluentDocument = Impl.empty(dir)

  object Serializers {
    implicit def group[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Group[S]] with evt.Reader[S, Group[S]] = {
      // implicit val spanType = SpanLikes
      BiGroup.Modifiable.serializer[S, Proc[S], Proc.Update[S]](_.changed)
    }
  }
}

sealed trait Document[S <: Sys[S]] {
  import Document.{Group => _}

  implicit def system: S
  // implicit def cursor: Cursor[S]
  // def aural: AuralSystem[S]
  def folder: File
  // def cursors: Cursors[S, S#D]

  // def masterCursor: stm.Cursor[S]

  type I <: SSys[I]
  implicit def inMemoryBridge: S#Tx => I#Tx
  implicit def inMemoryCursor: stm.Cursor[I]

  def root(implicit tx: S#Tx): Folder[S]

  def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A]

  implicit def systemType: reflect.runtime.universe.TypeTag[S]
}

trait ConfluentDocument extends Document[proc.Confluent] {
  type S = proc.Confluent

  // have to restate this for some reason?
  // cf. http://stackoverflow.com/questions/16495522/pattern-matching-refuses-to-recognize-member-type-value-x-is-not-a-member-of-2
  // def system: S

  def cursors: Cursors[S, S#D]
}

trait EphemeralDocument extends Document[proc.Durable] {
  type S = proc.Durable

  // def system: S

  def cursor: stm.Cursor[S] = system
}