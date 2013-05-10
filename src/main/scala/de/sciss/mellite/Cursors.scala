package de.sciss
package mellite

import lucre.{event => evt, stm, data}
import collection.immutable.{IndexedSeq => IIdxSeq}
import lucre.event.{DurableLike => DSys, Sys}
import lucre.confluent.reactive.{ConfluentReactiveLike => KSys}
import impl.{CursorsImpl => Impl}
import de.sciss.lucre.expr.Expr

object Cursors {
  def apply[S <: KSys[S], D1 <: DSys[D1]](seminal: S#Acc)
                                         (implicit tx: D1#Tx, system: S { type D = D1 }): Cursors[S, D1] =
    Impl[S, D1](seminal)


  implicit def serializer[S <: KSys[S], D1 <: DSys[D1]](implicit system: S { type D = D1 }):
    serial.Serializer[D1#Tx, D1#Acc, Cursors[S, D1]] with evt.Reader[D1, Cursors[S, D1]] = Impl.serializer[S, D1]

  final case class Update[S <: Sys[S], D <: Sys[D]](source: Cursors[S, D], changes: IIdxSeq[Change[S, D]])

  // final case class Advanced[S <: Sys[S], D <: Sys[D]](source: Cursors[S, D], change: evt.Change[S#Acc])
  //   extends Update[S, D]

  sealed trait Change[S <: Sys[S], D <: Sys[D]]

  final case class Renamed     [S <: Sys[S], D <: Sys[D]](change: evt.Change[String]) extends Change[S, D]
  final case class ChildAdded  [S <: Sys[S], D <: Sys[D]](child: Cursors[S, D])       extends Change[S, D]
  final case class ChildRemoved[S <: Sys[S], D <: Sys[D]](child: Cursors[S, D])       extends Change[S, D]
  final case class ChildUpdate [S <: Sys[S], D <: Sys[D]](change: Update[S, D])       extends Change[S, D]
}
trait Cursors[S <: Sys[S], D <: Sys[D]] extends serial.Writable {
  def seminal: S#Acc
  def cursor: stm.Cursor[S]

  def name(implicit tx: D#Tx): Expr[D, String]
  def name_=(value: Expr[D, String])(implicit tx: D#Tx): Unit

  // def children: expr.LinkedList.Modifiable[D, Cursors[S, D], Unit]

  def descendants(implicit tx: D#Tx): data.Iterator[D#Tx, Cursors[S, D]]

  def addChild(seminal: S#Acc)(implicit tx: D#Tx): Cursors[S, D]
  def removeChild(child: Cursors[S, D])(implicit tx: D#Tx): Unit

  def changed: evt.EventLike[D, Cursors.Update[S, D], Cursors[S, D]]
}