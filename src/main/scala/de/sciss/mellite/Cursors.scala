package de.sciss
package mellite

import de.sciss.lucre.{event => evt, confluent, data}
import collection.immutable.{IndexedSeq => Vec}
import lucre.event.{DurableLike => DSys}
import lucre.confluent.reactive.{ConfluentReactiveLike => KSys}
import impl.{CursorsImpl => Impl}
import de.sciss.lucre.expr.Expr

object Cursors {
  def apply[S <: KSys[S], D1 <: DSys[D1]](seminal: S#Acc)
                                         (implicit tx: D1#Tx, system: S { type D = D1 }): Cursors[S, D1] =
    Impl[S, D1](seminal)


  implicit def serializer[S <: KSys[S], D1 <: DSys[D1]](implicit system: S { type D = D1 }):
    serial.Serializer[D1#Tx, D1#Acc, Cursors[S, D1]] with evt.Reader[D1, Cursors[S, D1]] = Impl.serializer[S, D1]

  final case class Update[S <: KSys[S], D <: DSys[D]](source: Cursors[S, D], changes: Vec[Change[S, D]])

  // final case class Advanced[S <: Sys[S], D <: Sys[D]](source: Cursors[S, D], change: evt.Change[S#Acc])
  //   extends Update[S, D]

  sealed trait Change[S <: KSys[S], D <: DSys[D]]

  final case class Renamed     [S <: KSys[S], D <: DSys[D]](change: evt.Change[String])     extends Change[S, D]
  final case class ChildAdded  [S <: KSys[S], D <: DSys[D]](idx: Int, child: Cursors[S, D]) extends Change[S, D]
  final case class ChildRemoved[S <: KSys[S], D <: DSys[D]](idx: Int, child: Cursors[S, D]) extends Change[S, D]
  final case class ChildUpdate [S <: KSys[S], D <: DSys[D]](change: Update[S, D])           extends Change[S, D]
}
trait Cursors[S <: KSys[S], D <: DSys[D]] extends serial.Writable {
  def seminal: S#Acc
  // def cursor: stm.Cursor[S]
  def cursor: confluent.Cursor[S, D]

  def name(implicit tx: D#Tx): Expr[D, String]
  def name_=(value: Expr[D, String])(implicit tx: D#Tx): Unit

  // def children: expr.LinkedList.Modifiable[D, Cursors[S, D], Unit]

  def descendants(implicit tx: D#Tx): data.Iterator[D#Tx, Cursors[S, D]]

  def addChild(seminal: S#Acc)(implicit tx: D#Tx): Cursors[S, D]
  def removeChild(child: Cursors[S, D])(implicit tx: D#Tx): Unit

  def changed: evt.EventLike[D, Cursors.Update[S, D], Cursors[S, D]]
}