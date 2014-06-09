/*
 *  Workspace.scala
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
import impl.{WorkspaceImpl => Impl}
import de.sciss.synth.proc.{Obj, Folder, Proc}
import de.sciss.serial.Serializer
import collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.stm.Disposable

object Workspace {
  /** File name extension (excluding leading period) */
  final val ext = "mllt"

  type Group       [S <: Sys[S]] = BiGroup.Modifiable   [S, Proc[S], Proc.Update[S]]
  type GroupUpdate [S <: Sys[S]] = BiGroup.Update       [S, Proc[S], Proc.Update[S]]

  type Groups      [S <: Sys[S]] = expr.List.Modifiable[S, Group[S], GroupUpdate[S]]
  type GroupsUpdate[S <: Sys[S]] = expr.List.Update    [S, Group[S], GroupUpdate[S]]

  type Transport   [S <: Sys[S]] = proc.ProcTransport[S]
  type Transports  [S <: Sys[S]] = expr.List.Modifiable[S, Transport[S], Unit] // Transport.Update[ S, Proc[ S ]]]

  def read (dir: File): Workspace[_] /* [~ forSome { type ~ <: SSys[~] }] */ = Impl.read(dir)

  object Confluent {
    def read (dir: File): Confluent = Impl.readConfluent (dir)
    def empty(dir: File): Confluent = Impl.emptyConfluent(dir)
  }

  trait Confluent extends Workspace[proc.Confluent] {
    type S = proc.Confluent

    // have to restate this for some reason?
    // cf. http://stackoverflow.com/questions/16495522/pattern-matching-refuses-to-recognize-member-type-value-x-is-not-a-member-of-2
    // def system: S

    def cursors: Cursors[S, S#D]
  }
  
  object Ephemeral {
    def read (dir: File): Ephemeral = Impl.readEphemeral (dir)
    def empty(dir: File): Ephemeral = Impl.emptyEphemeral(dir)
  }
  trait Ephemeral extends Workspace[proc.Durable] {
    type S = proc.Durable

    // def system: S

    def cursor: stm.Cursor[S] = system
  }

  object Serializers {
    implicit def group[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Group[S]] with evt.Reader[S, Group[S]] = {
      // implicit val spanType = SpanLikes
      BiGroup.Modifiable.serializer[S, Proc[S], Proc.Update[S]](_.changed)
    }
  }
}

sealed trait Workspace[S <: Sys[S]] {
  import Workspace.{Group => _}

  // type S1 = S

  implicit def system: S
  // implicit def cursor: Cursor[S]
  // def aural: AuralSystem[S]
  def folder: File
  // def cursors: Cursors[S, S#D]

  // def masterCursor: stm.Cursor[S]

  type I <: SSys[I]
  implicit def inMemoryBridge: S#Tx => I#Tx
  implicit def inMemoryCursor: stm.Cursor[I]

  // def root(implicit tx: S#Tx): Folder[S]

  def root: stm.Source[S#Tx, Folder[S]]

  def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A]

  implicit def systemType: reflect.runtime.universe.TypeTag[S]

  /** Adds a dependent which is disposed just before the workspace is disposed.
    *
    * @param dep  the dependent. This must be an _ephemeral_ object.
    */
  def addDependent   (dep: Disposable[S#Tx])(implicit tx: S#Tx): Unit
  def removeDependent(dep: Disposable[S#Tx])(implicit tx: S#Tx): Unit
}