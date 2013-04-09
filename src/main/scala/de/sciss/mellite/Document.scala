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
import stm.Cursor
import de.sciss.synth.expr.SpanLikes
import de.sciss.serial.Serializer

object Document {
   type Group[        S <: Sys[ S ]]   = BiGroup.Modifiable[    S, Proc[ S ],  Proc.Update[ S ]]
//   type GroupU[       S <: Sys[ S ]]   = BiGroup[    S, Proc[ S ],  Proc.Update[ S ]]
   type GroupUpdate[  S <: Sys[ S ]]   = BiGroup.Update[        S, Proc[ S ],  Proc.Update[ S ]]
   type Groups[       S <: Sys[ S ]]   = LinkedList.Modifiable[ S, Group[ S ], GroupUpdate[ S ]]
   type GroupsUpdate[ S <: Sys[ S ]]   = LinkedList.Update[     S, Group[ S ], GroupUpdate[ S ]]

   type Transport[    S <: Sys[ S ]]   = proc.ProcTransport[ S ]
   type Transports[   S <: Sys[ S ]]   = LinkedList.Modifiable[ S, Transport[ S ], Unit ] // Transport.Update[ S, Proc[ S ]]]

   def read(  dir: File ) : Document[ Cf ] = Impl.read( dir )
   def empty( dir: File ) : Document[ Cf ] = Impl.empty( dir )

   object Serializers {
      implicit def group[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, Group[ S ]] with evt.Reader[ S, Group[ S ]] = {
         implicit val spanType = SpanLikes
         BiGroup.Modifiable.serializer[ S, Proc[ S ], Proc.Update[ S ]]( _.changed )
      }

//      implicit def groupU[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, GroupU[ S ]] with evt.Reader[ S, GroupU[ S ]] = {
//         implicit val spanType = SpanLikes
//         BiGroup.serializer[ S, Proc[ S ], Proc.Update[ S ]]( _.changed )
//      }

//      implicit def groups[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, LinkedList[ S, Group[ S ], GroupUpdate[ S ]]] = {
//         LinkedList.serializer[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )
//      }

//      implicit def groups[ S <: Sys[ S ]] : Serializer[ S#Tx, S#Acc, LinkedList[ S, GroupU[ S ], GroupUpdate[ S ]]] = {
//         LinkedList.serializer[ S, GroupU[ S ], GroupUpdate[ S ]]( _.changed )
//      }

//      implicit def transports[ S <: Sys[ S ]]( implicit cursor: Cursor[ S ]) : Serializer[ S#Tx, S#Acc, LinkedList[ S, Transport[ S ], Unit ]] = {
////         implicit val elem = proc.Transport.serializer[ S ]
//         LinkedList.serializer[ S, Transport[ S ]]
//      }
   }
}

trait Document[S <: Sys[S]] {

  import Document.{Group => _, _}

  def system: S
  def cursor: Cursor[S]
  def aural: AuralSystem[S]
  def folder: File

  //  def root(implicit tx: S#Tx): Group[S]
  //  def groups( implicit tx: S#Tx ) : Groups[S]
  //   def transports( group: Group[ S ])( implicit tx: S#Tx ) : Transports[ S ]

  def elements(implicit tx: S#Tx): Folder[S]

  // def manifest: reflect.runtime.universe.TypeTag[Document[S]]
  implicit def systemType: reflect.runtime.universe.TypeTag[S]

  //   def exprImplicits: ExprImplicits[ S ]
}
