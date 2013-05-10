package de.sciss
package mellite

import lucre.{event => evt, stm, expr, data}
import collection.immutable.{IndexedSeq => IIdxSeq}
import evt.Sys

object Cursors {
  final case class Update[S <: Sys[S], D <: Sys[D]](source: Cursors[S, D], changes: IIdxSeq[Change[S, D]])

  // final case class Advanced[S <: Sys[S], D <: Sys[D]](source: Cursors[S, D], change: evt.Change[S#Acc])
  //   extends Update[S, D]

  sealed trait Change[S <: Sys[S], D <: Sys[D]]

  final case class ChildAdded  [S <: Sys[S], D <: Sys[D]](child: Cursors[S, D]) extends Change[S, D]
  final case class ChildRemoved[S <: Sys[S], D <: Sys[D]](child: Cursors[S, D]) extends Change[S, D]
  final case class ChildUpdate [S <: Sys[S], D <: Sys[D]](change: Update[S, D]) extends Change[S, D]
}
trait Cursors[S <: Sys[S], D <: Sys[D]] extends serial.Writable {
  def seminal: S#Acc
  def cursor: stm.Cursor[S]

  // def children: expr.LinkedList.Modifiable[D, Cursors[S, D], Unit]

  def descendants(implicit tx: D#Tx): data.Iterator[D#Tx, Cursors[S, D]]

  def addChild(seminal: S#Acc)(implicit tx: D#Tx): Cursors[S, D]
  def removeChild(child: Cursors[S, D])(implicit tx: D#Tx): Unit

  def changed: evt.EventLike[D, Cursors.Update[S, D], Cursors[S, D]]
}