/*
 *  DocumentImpl.scala
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
package impl

import java.io.{IOException, FileNotFoundException, File}
import de.sciss.confluent.Confluent
import de.sciss.lucre.{event => evt, expr, bitemp, stm, DataOutput, DataInput}
import expr.LinkedList
import bitemp.BiGroup
import stm.{SourceHook, IdentifierMap, Cursor, Sys, TxnSerializer}
import stm.impl.BerkeleyDB
import de.sciss.synth.expr.SpanLikes
import de.sciss.synth.proc.{Transport, Proc}

object DocumentImpl {
   import Document.{Group, GroupUpdate, Groups, Transports}

   private def procSer[   S <: Sys[ S ]]  = Proc.serializer[ S ]
   private def groupSer[  S <: Sys[ S ]]  = BiGroup.Modifiable.serializer[    S, Proc[ S ],  Proc.Update[ S ]]( _.changed )( procSer, SpanLikes )
   private def groupsSer[ S <: Sys[ S ]]  = LinkedList.Modifiable.serializer[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )( groupSer )

//   private def transportSer[ S <: Sys[ S ]] = Transport.serializer[ S, Group[ S ]]()

   private implicit def serializer[ S <: Sys[ S ]] : TxnSerializer[ S#Tx, S#Acc, Data[ S ]] = new Ser[ S ]

   private final class Ser[ S <: Sys[ S ]] extends TxnSerializer[ S#Tx, S#Acc, Data[ S ]] {
      def write( data: Data[ S ], out: DataOutput ) {
         data.write( out )
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Data[ S ] = new Data[ S ] {
         val groups = groupsSer.read( in, access )
      }
   }

   def read( dir: File ) : Document[ Cf ] = {
      if( !dir.isDirectory ) throw new FileNotFoundException( "Document " + dir.getPath + " does not exist" )
      apply( dir, create = false )
   }

   def empty( dir: File ) : Document[ Cf ] = {
      if( dir.exists() ) throw new IOException( "Document " + dir.getPath + " already exists" )
      apply( dir, create = true )
   }

   private def apply( dir: File, create: Boolean ) : Document[ Cf ] = {
      val fact    = BerkeleyDB.factory( dir, createIfNecessary = create )
      val system  = Confluent( fact )
      implicit val serializer    = DocumentImpl.serializer[ Cf ]  // please Scala 2.9.2 and 2.10.0-M6 :-////
      implicit val transportSer  = Transport.serializer[ Cf ]( system )
      implicit val transportsSer = LinkedList.Modifiable.serializer[ Cf, Transport[ Cf, Proc[ Cf ]]]
      val access  = system.root[ Data[ Cf ]] { implicit tx =>
         new Data[ Cf ] {
            val groups        = LinkedList.Modifiable[ Cf, Group[ Cf ], GroupUpdate[ Cf ]]( _.changed )( tx, groupSer[ Cf ])
            val transportMap  = tx.newDurableIDMap[ Transports[ Cf ]]
         }
      }
      new Impl( dir, system, system, access )
   }

//   private def dummyEvent[ S <: Sys[ S ]] = evt.Dummy[ S, Unit, SourceHook[ S#Tx, Transport[ S, Proc[ S ]]]]

   private abstract class Data[ S <: Sys[ S ]] {
      def groups: Groups[ S ]
      def transportMap: IdentifierMap[ S#Tx, S#ID, Transports[ S ]]

      final def write( out: DataOutput ) {
         groups.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         groups.dispose()
      }
   }

   private final class Impl[ S <: Sys[ S ]]( val folder: File, val system: S, val cursor: Cursor[ S ], access: S#Entry[ Data[ S ]])
   extends Document[ S ] {
      override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

      def groups( implicit tx: S#Tx ) : Groups[ S ] = access.get.groups
      def transports( group: Group[ S ])( implicit tx: S#Tx ) : Transports[ S ] = {
         val map  = access.get.transportMap
         val id   = group.id
         map.getOrElse( id, {
            val empty = LinkedList.Modifiable[ S, Transport[ S, Proc[ S ]]]
            map.put( id, empty )
            empty
         })
      }
   }
}