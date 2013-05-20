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
import de.sciss.synth.proc.Confluent
import de.sciss.serial.{DataInput, Serializer, DataOutput}
import de.sciss.synth.expr.ExprImplicits

object DocumentImpl {
  private type S = Cf

  private implicit object serializer extends Serializer[S#Tx, S#Acc, Data] {
    def write(data: Data, out: DataOutput) {
      data.write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data = {
      val cookie = in.readLong()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (should be $COOKIE)")
      new Data {
        val elements  = Folder.read[S](in, access)
      // val cursor    = ... // tx.readCursor(in, access)
      }
    }
  }

  def read(dir: File): ConfluentDocument = {
    if (!dir.isDirectory) throw new FileNotFoundException("Document " + dir.getPath + " does not exist")
    apply(dir, create = false)
  }

  def empty(dir: File): ConfluentDocument = {
    if (dir.exists()) throw new IOException("Document " + dir.getPath + " already exists")
    apply(dir, create = true)
  }

  private def apply(dir: File, create: Boolean): ConfluentDocument = {
    type S    = Cf
    val fact  = BerkeleyDB.factory(dir, createIfNecessary = create)
    implicit val system: S = Confluent(fact)

    val (access, cursors) = system.rootWithDurable[Data, Cursors[S, S#D]] { implicit tx =>
      val data: Data = new Data {
        val elements  = Folder[S](tx)
        // val cursor    = system.newCursor()
      }
      data

    } { implicit tx =>
      val c   = Cursors[S, S#D](confluent.Sys.Acc.root[S])
      val imp = ExprImplicits[S#D]
      import imp._
      c.name_=("master")
      c
    }

    implicit val cfTpe  = reflect.runtime.universe.typeOf[Cf]
    new Impl(dir, system, access, cursors)
  }

  private final val COOKIE = 0x4D656C6C69746500L  // "Mellite\0"

  private abstract class Data {
    def elements: Folder[S]
    // def cursor: confluent.Cursor[S, S#D]

    final def write(out: DataOutput) {
      out.writeLong(COOKIE)
      elements.write(out)
      // cursor  .write(out)
    }

    final def dispose()(implicit tx: S#Tx) {
      elements.dispose()
      // cursor  .dispose()
    }

    override def toString = s"Data ($elements)"
  }

  private final class Impl(val folder: File, val system: S, access: stm.Source[S#Tx, Data],
                           val cursors: Cursors[S, S#D])
                          (implicit val systemType: reflect.runtime.universe.TypeTag[S])
    extends ConfluentDocument {
    override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

    def elements(implicit tx: S#Tx): Folder[S] = access().elements

    type I = system.I
    val inMemoryBridge = (tx: S#Tx) => Confluent.inMemory(tx)
    def inMemoryCursor: stm.Cursor[I] = system.inMemory
  }
}