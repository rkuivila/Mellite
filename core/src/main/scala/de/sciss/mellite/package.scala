/*
 *  package.scala
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

package de.sciss

import de.sciss.mellite.impl.{AuralActionImpl, ActionImpl, CodeImpl}
import de.sciss.synth.proc.Confluent
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.concurrent.ExecutionContext

package object mellite {
  type Cf = Confluent

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Mellite' - ", Locale.US)
  var showLog         = false
  var showTimelineLog = false

  @elidable(CONFIG) private[mellite] def log(what: => String): Unit =
    if (showLog) println(logHeader.format(new Date()) + what)

  @elidable(CONFIG) private[mellite] def logTimeline(what: => String): Unit =
    if (showTimelineLog) println(s"${logHeader.format(new Date())} <timeline> $what")

  implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  def initTypes(): Unit = {
    de.sciss.lucre.synth.expr.initTypes()
    CodeImpl.ElemImpl
    // RecursionImpl.ElemImpl
    ActionImpl.ElemImpl
    AuralActionImpl
  }

  //  object Folder {
  //    import mellite.{Element => _Element}
  //
  //    def apply[S <: Sys[S]](implicit tx: S#Tx): Folder[S] = expr.List.Modifiable[S, _Element[S], _Element.Update[S]]
  //
  //    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Folder[S] =
  //      expr.List.Modifiable.read[S, _Element[S], _Element.Update[S]](in, access)
  //
  //    //    object Update {
  //    //      def unapply[S <: Sys[S]](upd: expr.List.Update[ S, _Element[S], _Element.Update[S]]) = Some((upd.list, upd.changes))
  //    //    }
  //    //
  //    //    object Added {
  //    //      def unapply[S <: Sys[S]](change: expr.List.Change[S, _Element[S], _Element.Update[S]]) = change match {
  //    //        case expr.List.Added(idx, elem) => Some((idx, elem))
  //    //        case _ => None
  //    //      }
  //    ////      def unapply[S <: Sys[S]](change: expr.List.Added[S, Element[S]]) = Some(change.index, change.elem)
  //    //    }
  //    //    object Removed {
  //    //      def unapply[S <: Sys[S]](change: expr.List.Change[S, _Element[S], _Element.Update[S]]) = change match {
  //    //        case expr.List.Removed(idx, elem) => Some((idx, elem))
  //    //        case _ => None
  //    //      }
  //    //    }
  //    //    object Element {
  //    //      def unapply[S <: Sys[S]](change: expr.List.Change[S, _Element[S], _Element.Update[S]]) = change match {
  //    //        case expr.List.Element(elem, elemUpd) => Some((elem, elemUpd))
  //    //        case _ => None
  //    //      }
  //    //    }
  //
  //    // private[Folder] type _Update[S <: Sys[S]] = expr.List.Update[S, _Element[S], _Element.Update[S]]
  //    type Update[S <: Sys[S]] = Vec[Change[S]]
  //    sealed trait Change[S <: Sys[S]] { def elem: _Element[S] }
  //    final case class Added  [S <: Sys[S]](idx: Int, elem: _Element[S]) extends Change[S]
  //    final case class Removed[S <: Sys[S]](idx: Int, elem: _Element[S]) extends Change[S]
  //    final case class Element[S <: Sys[S]](elem: _Element[S], update: _Element.Update[S]) extends Change[S]
  //
  //    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Folder[S]] =
  //      anySer.asInstanceOf[Serializer[S#Tx, S#Acc, Folder[S]]]
  //
  //    private val anySer: Serializer[InMemory#Tx, InMemory#Acc, Folder[InMemory]] =
  //      expr.List.Modifiable.serializer[InMemory, _Element[InMemory], _Element.Update[InMemory]]
  //  }
  // type Folder[S <: Sys[S]] = expr.List.Modifiable[S, Element[S], Element.Update[S]]
}