/*
 *  DocumentImpl.scala
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
package impl

import java.io.{IOException, FileNotFoundException, File}
import de.sciss.lucre.{confluent, stm}
import stm.store.BerkeleyDB
import stm.Cursor
import de.sciss.synth.proc.{AuralSystem, Confluent}
import de.sciss.serial.{DataInput, Serializer, DataOutput}

object DocumentImpl {
  private type S = Cf

  private implicit object serializer extends Serializer[S#Tx, S#Acc, Data /* [S] */] {
    def write(data: Data /* [S] */, out: DataOutput) {
      data.write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data /* [S] */ = new Data /* [S] */ {
      val elements = Folder.read[S](in, access)
      // elementsSer.read( in, access )
      val cursor = tx.readCursor(in, access)
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

  private def apply(dir: File, create: Boolean): Document[Cf] = {
    type S    = Cf
    val fact  = BerkeleyDB.factory(dir, createIfNecessary = create)
    implicit val system: S = Confluent(fact)
    //    implicit val serializer = DocumentImpl.serializer[S] // please Scala 2.9.2 and 2.10.0-M6 :-////
    val (_access, _cursor) = system.cursorRoot[Data /* [S] */, Cursor[S]](implicit tx =>
        new Data /* [S] */ {
          //            val groups        = LinkedList.Modifiable[ S, Group[ S ], GroupUpdate[ S ]]( _.changed )( tx, groupSer[ S ])
          val elements  = Folder[S](tx)
          val cursor    = tx.newCursor()
          //println("DOC.new")

          //          // test
          //          private val g1: Elements[S] = LinkedList.Modifiable[S, Element[S]](tx, Element.serializer)
          //          g1.addLast(Element.String(Strings.newConst("string-value"), Some("s1")))
          //          private val eg1 = Element.Group(g1, Some("g1"))
          //          elements.addLast(eg1)
        }
      )(tx => _.cursor)
    val access: S#Entry[Data /* [S] */] = _access
    implicit val cursor: Cursor[S] = _cursor
    //println("CURSOR = " + cursor)
    implicit val cfTpe = reflect.runtime.universe.typeOf[Cf /* S */]
    new Impl /* [ Cf /* S */] */(dir, system, access)
  }

  private abstract class Data /*[S <: confluent.Sys[S]] */ {
    def elements: Folder[S]
    def cursor: confluent.Cursor[S]

    final def write(out: DataOutput) {
      elements.write(out)
      cursor.write(out)
      }

    final def dispose()(implicit tx: S#Tx) {
      elements.dispose()
      cursor.dispose()
    }
  }

  private final class Impl /* [S <: Sys[S]] */(val folder: File, val system: S, access: S#Entry[Data/* [S] */])
                                       (implicit val cursor: Cursor[S], val systemType: reflect.runtime.universe.TypeTag[S])
    extends Document[S] {
    override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

    def elements(implicit tx: S#Tx): Folder[S] = access().elements

    def aural: AuralSystem[S] = ???
  }
}