package de.sciss.mellite
package impl

import java.io.File
import de.sciss.lucre.stm.impl.BerkeleyDB
import de.sciss.confluent.Confluent
import de.sciss.lucre.stm.{Writer, TxnSerializer}
import de.sciss.lucre.{DataOutput, DataInput}
import de.sciss.synth.proc.Proc
import de.sciss.lucre.expr.{LinkedList, BiGroup}
import de.sciss.synth.expr.{SpanLikes, Spans}

object DocumentImpl {
   import Document.{Group, GroupUpdate, Groups, GroupsUpdate}

   private val procSer     = Proc.serializer[ Cf ]
   private val groupSer    = BiGroup.varSerializer[ Cf, Proc[ Cf ], Proc.Update[ Cf ]]( _.changed )( procSer, SpanLikes )
   private val groupsSer   = LinkedList.varSerializer[ Cf, Group, GroupUpdate ]( _.changed )( groupSer )

   private implicit object serializer extends TxnSerializer[ Cf#Tx, Cf#Acc, Data ] {
      def write( data: Data, out: DataOutput ) {
         data.write( out )
      }

      def read( in: DataInput, access:Cf#Acc )( implicit tx: Cf#Tx ) : Data = new Data {
         val groups = groupsSer.read( in, access )
      }
   }

   def empty( dir: File ) : Document = {
      val fact    = BerkeleyDB.factory( dir, createIfNecessary = true )
      val system  = Confluent( fact )
      val access  = system.root[ Data ] { implicit tx =>
         new Data {
         }
      }
      new Impl( dir, system, access )
   }

   private abstract class Data {
      def groups: Groups

      final def write( out: DataOutput ) {
         groups.write( out )
      }

      final def dispose()( implicit tx: Cf#Tx ) {
         groups.dispose()
      }
   }

   private final class Impl( val folder: File, val system: Cf, access: Cf#Entry[ Data ])
   extends Document {
      override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

      def groups( implicit tx: Cf#Tx ) : Groups =
         access.get.groups
   }
}
