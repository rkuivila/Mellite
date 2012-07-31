/*
 *  Document.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012 Hanns Holger Rutz. All rights reserved.
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
import proc.Proc
import stm.{TxnSerializer, Cursor, Sys}
import de.sciss.synth.expr.{ExprImplicits, SpanLikes}

object Document {
   type Group[        S <: Sys[ S ]]   = BiGroup.Modifiable[    S, Proc[ S ],  Proc.Update[ S ]]
   type GroupUpdate[  S <: Sys[ S ]]   = BiGroup.Update[        S, Proc[ S ],  Proc.Update[ S ]]
   type Groups[       S <: Sys[ S ]]   = LinkedList.Modifiable[ S, Group[ S ], GroupUpdate[ S ]]
   type GroupsUpdate[ S <: Sys[ S ]]   = LinkedList.Update[     S, Group[ S ], GroupUpdate[ S ]]

   type Transport[    S <: Sys[ S ]]   = proc.Transport[ S, Proc[ S ]]
   type Transports[   S <: Sys[ S ]]   = LinkedList.Modifiable[ S, Transport[ S ], Unit ] // Transport.Update[ S, Proc[ S ]]]

   def read(  dir: File ) : Document[ Cf ] = Impl.read( dir )
   def empty( dir: File ) : Document[ Cf ] = Impl.empty( dir )

   object Serializers {
      implicit def group[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, Group[ S ]] with evt.Reader[ S, Group[ S ]] = {
         implicit val spanType = SpanLikes
         BiGroup.Modifiable.serializer[ S, Proc[ S ], Proc.Update[ S ]]( _.changed )
      }

      implicit def groups[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, LinkedList[ S, Group[ S ], GroupUpdate[ S ]]] = {
         LinkedList.serializer[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )
      }

      implicit def transports[ S <: Sys[ S ]]( implicit cursor: Cursor[ S ]) : TxnSerializer[ S#Tx, S#Acc, LinkedList[ S, Transport[ S ], Unit ]] = {
//         implicit val elem = proc.Transport.serializer[ S ]
         LinkedList.serializer[ S, Transport[ S ]]
      }
   }
}
trait Document[ S <: Sys[ S ]] {
   import Document._

   def system: S
   def cursor: Cursor[ S ]
   def folder: File
   def groups( implicit tx: S#Tx ) : Groups[ S ]
   def transports( group: Group[ S ])( implicit tx: S#Tx ) : Transports[ S ]

//   def exprImplicits: ExprImplicits[ S ]
}
