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
import de.sciss.lucre.{expr, bitemp, stm, DataOutput, DataInput}
import expr.LinkedList
import bitemp.BiGroup
import stm.store.BerkeleyDB
import stm.{Cursor, Serializer}
import de.sciss.synth.expr.SpanLikes
import de.sciss.synth.proc.{Sys, Confluent, Transport, Proc}

object DocumentImpl {
   import Document.{Group, GroupUpdate, Groups, Transports}

   private def procSer[   S <: Sys[ S ]]  = Proc.serializer[ S ]
   private def groupSer[  S <: Sys[ S ]]  = BiGroup.Modifiable.serializer[    S, Proc[ S ],  Proc.Update[ S ]]( _.changed )( procSer, SpanLikes )
   private def groupsSer[ S <: Sys[ S ]]  = LinkedList.Modifiable.serializer[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )( groupSer )

//   private def transportSer[ S <: Sys[ S ]] = Transport.serializer[ S, Group[ S ]]()

   private implicit def serializer[ S <: Sys[ S ]] /* ( implicit cursor: Cursor[ S ]) */ : Serializer[ S#Tx, S#Acc, Data[ S ]] = new Ser[ S ]

   private final class Ser[ S <: Sys[ S ]] /*( implicit cursor: Cursor[ S ]) */ extends Serializer[ S#Tx, S#Acc, Data[ S ]] {
      def write( data: Data[ S ], out: DataOutput ) {
         data.write( out )
      }

      def read( in: DataInput, access: S#Acc )( implicit tx: S#Tx ) : Data[ S ] = new Data[ S ] {
         val groups                 = groupsSer.read( in, access )
//         implicit val transSer      = Transport.serializer[ S ]  // why is this not found automatically??
//         implicit val transportsSer = LinkedList.Modifiable.serializer[ S, Transport[ S, Proc[ S ]]]
//         val transportMap           = tx.readDurableIDMap[ Transports[ S ]]( in )
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
      type S = Cf
      val fact                   = BerkeleyDB.factory( dir, createIfNecessary = create )
      implicit val system: S     = Confluent( fact )
      implicit val serializer    = DocumentImpl.serializer[ S ]   // please Scala 2.9.2 and 2.10.0-M6 :-////
//      implicit val transSer      = Transport.serializer[ S ]      // why is this not found automatically??
//      implicit val transportsSer = LinkedList.Modifiable.serializer[ S, Transport[ S, Proc[ S ]]]
//      val access = system.root[ Data[ S ]] { implicit tx =>
//         new Data[ S ] {
//            val groups        = LinkedList.Modifiable[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )( tx, groupSer[ S ])
////            val transportMap  = tx.newDurableIDMap[ Transports[ S ]]
//         }
//      }
      val (_access, _cursor) = system.cursorRoot[ Data[ S ], Cursor[ S ]]( implicit tx =>
         new Data[ S ] {
            val groups        = LinkedList.Modifiable[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )( tx, groupSer[ S ])
//            val transportMap  = tx.newDurableIDMap[ Transports[ S ]]
         }
      )( tx => _ => tx.newCursor() )
      val access: S#Entry[ Data[ S ]] = _access
      implicit val cursor: Cursor[ S ] = _cursor
      new Impl[ S ]( dir, system, access )
   }

   private abstract class Data[ S <: Sys[ S ]] {
      def groups: Groups[ S ]
//      def transportMap: IdentifierMap[ S#ID, S#Tx, Transports[ S ]]

      final def write( out: DataOutput ) {
         groups.write( out )
//         transportMap.write( out )
      }

      final def dispose()( implicit tx: S#Tx ) {
         groups.dispose()
//         transportMap.dispose()
      }
   }

   private final class Impl[ S <: Sys[ S ]]( val folder: File, val system: S, access: S#Entry[ Data[ S ]])
                                           ( implicit val cursor: Cursor[ S ])
   extends Document[ S ] {
      override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

      def groups( implicit tx: S#Tx ) : Groups[ S ] = access.get.groups
//      def transports( group: Group[ S ])( implicit tx: S#Tx ) : Transports[ S ] = {
//         val map  = access.get.transportMap
//         val id   = group.id
//         map.getOrElse( id, {
//            val empty = LinkedList.Modifiable[ S, Transport[ S, Proc[ S ]]]
//            map.put( id, empty )
//            empty
//         })
//      }
   }
}