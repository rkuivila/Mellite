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

import lucre.{stm, expr, io}
import expr.LinkedList
import io.DataInput
import synth.proc.{InMemory, Sys, Confluent}

package object mellite {
  type Cf           = Confluent
//   type S            = Confluent
//   type Ex[ A ]      = expr.Expr[ S, A ]
//   object Ex {
//      type Var[ A ] = expr.Expr.Var[ S, A ]
//   }

//   type Elements[ S <: Sys[ S ]] = LinkedList.Modifiable[ S, Element[ S, _ ], Any ]
  object Elements {
    import mellite.{Element => _Element}

    def apply[S <: Sys[S]](implicit tx: S#Tx): Elements[S] = LinkedList.Modifiable[S, _Element[S], _Element.Update[S]](_.changed)

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Elements[S] =
      LinkedList.Modifiable.read[S, _Element[S], _Element.Update[S]](_.changed)(in, access)

    object Update {
      def unapply[S <: Sys[S]](upd: LinkedList.Update[ S, _Element[S], _Element.Update[S]]) = Some((upd.list, upd.changes))
    }

    object Added {
      def unapply[S <: Sys[S]](change: LinkedList.Change[S, _Element[S], _Element.Update[S]]) = change match {
        case LinkedList.Added(idx, elem) => Some((idx, elem))
        case _ => None
      }
//      def unapply[S <: Sys[S]](change: LinkedList.Added[S, Element[S]]) = Some(change.index, change.elem)
    }
    object Removed {
      def unapply[S <: Sys[S]](change: LinkedList.Change[S, _Element[S], _Element.Update[S]]) = change match {
        case LinkedList.Removed(idx, elem) => Some((idx, elem))
        case _ => None
      }
    }
    object Element {
      def unapply[S <: Sys[S]](change: LinkedList.Change[S, _Element[S], _Element.Update[S]]) = change match {
        case LinkedList.Element(elem, elemUpd) => Some((elem, elemUpd))
        case _ => None
      }
    }

    type Update[S <: Sys[S]] = LinkedList.Update[S, _Element[S], _Element.Update[S]]

    implicit def serializer[S <: Sys[S]]: io.Serializer[S#Tx, S#Acc, Elements[S]] =
      anySer.asInstanceOf[io.Serializer[S#Tx, S#Acc, Elements[S]]]

    private val anySer: io.Serializer[InMemory#Tx, InMemory#Acc, Elements[InMemory]] =
      LinkedList.Modifiable.serializer[InMemory, Element[InMemory], mellite.Element.Update[InMemory]](_.changed)
  }
  type Elements[S <: Sys[S]] = LinkedList.Modifiable[S, Element[S], Element.Update[S]]
}