/*
 *  package.scala
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

package de.sciss

import lucre.expr
import expr.LinkedList
import synth.proc.{InMemory, Sys, Confluent}
import de.sciss.serial.{Serializer, DataInput}
import scala.collection.immutable.{IndexedSeq => IIdxSeq}
import java.io.File
import java.text.SimpleDateFormat
import java.util.{Date, Locale}
import scala.annotation.elidable
import scala.annotation.elidable.CONFIG
import scala.concurrent.ExecutionContext

package object mellite {
  type Cf           = Confluent

  private lazy val logHeader = new SimpleDateFormat("[d MMM yyyy, HH:mm''ss.SSS] 'Mellite' - ", Locale.US)
  var showLog = false

  @elidable(CONFIG) private[mellite] def log(what: => String) {
    if (showLog) {
      println(logHeader.format(new Date()) + what)
    }
  }

  implicit val executionContext: ExecutionContext = ExecutionContext.Implicits.global

  object Folder {
    import mellite.{Element => _Element}

    def apply[S <: Sys[S]](implicit tx: S#Tx): Folder[S] = LinkedList.Modifiable[S, _Element[S], _Element.Update[S]](_.changed)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Folder[S] =
      LinkedList.Modifiable.read[S, _Element[S], _Element.Update[S]](_.changed)(in, access)

    //    object Update {
    //      def unapply[S <: Sys[S]](upd: LinkedList.Update[ S, _Element[S], _Element.Update[S]]) = Some((upd.list, upd.changes))
    //    }
    //
    //    object Added {
    //      def unapply[S <: Sys[S]](change: LinkedList.Change[S, _Element[S], _Element.Update[S]]) = change match {
    //        case LinkedList.Added(idx, elem) => Some((idx, elem))
    //        case _ => None
    //      }
    ////      def unapply[S <: Sys[S]](change: LinkedList.Added[S, Element[S]]) = Some(change.index, change.elem)
    //    }
    //    object Removed {
    //      def unapply[S <: Sys[S]](change: LinkedList.Change[S, _Element[S], _Element.Update[S]]) = change match {
    //        case LinkedList.Removed(idx, elem) => Some((idx, elem))
    //        case _ => None
    //      }
    //    }
    //    object Element {
    //      def unapply[S <: Sys[S]](change: LinkedList.Change[S, _Element[S], _Element.Update[S]]) = change match {
    //        case LinkedList.Element(elem, elemUpd) => Some((elem, elemUpd))
    //        case _ => None
    //      }
    //    }

    // private[Folder] type _Update[S <: Sys[S]] = LinkedList.Update[S, _Element[S], _Element.Update[S]]
    type Update[S <: Sys[S]] = IIdxSeq[Change[S]]
    sealed trait Change[S <: Sys[S]] { def elem: _Element[S] }
    final case class Added  [S <: Sys[S]](idx: Int, elem: _Element[S]) extends Change[S]
    final case class Removed[S <: Sys[S]](idx: Int, elem: _Element[S]) extends Change[S]
    final case class Element[S <: Sys[S]](elem: _Element[S], update: _Element.Update[S]) extends Change[S]

    implicit def serializer[S <: Sys[S]]: Serializer[S#Tx, S#Acc, Folder[S]] =
      anySer.asInstanceOf[Serializer[S#Tx, S#Acc, Folder[S]]]

    private val anySer: Serializer[InMemory#Tx, InMemory#Acc, Folder[InMemory]] =
      LinkedList.Modifiable.serializer[InMemory, _Element[InMemory], _Element.Update[InMemory]](_.changed)
  }
  type Folder[S <: Sys[S]] = LinkedList.Modifiable[S, Element[S], Element.Update[S]]

  implicit class RichFile(f: File) {
    def /(child: String): File = new File(f, child)
    def path: String  = f.getPath
    def name: String  = f.getName
    def parent: File  = f.getParentFile
    def nameWithoutExtension: String = {
      val n = f.getName
      val i = n.lastIndexOf('.')
      if (i < 0) n else n.substring(0, i)
    }
    def replaceExtension(s: String): File = {
      val n   = nameWithoutExtension
      val ext = if (s.charAt(0) == '.') s else "." + s
      new File(f.getParentFile, n + ext)
    }
  }
}