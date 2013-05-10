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

  private implicit object serializer extends Serializer[S#Tx, S#Acc, Data] {
    def write(data: Data, out: DataOutput) {
      data.write(out)
    }

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data = new Data {
      val elements  = Folder.read[S](in, access)
      val cursor    = tx.readCursor(in, access)
    }
  }

  def read(dir: File): Document[Cf] = {
    if (!dir.isDirectory) throw new FileNotFoundException("Document " + dir.getPath + " does not exist")
    apply(dir, create = false)
  }

  def empty(dir: File): Document[Cf] = {
    if (dir.exists()) throw new IOException("Document " + dir.getPath + " already exists")
    apply(dir, create = true)
  }

  private def apply(dir: File, create: Boolean): Document[Cf] = {
    type S    = Cf
    val fact  = BerkeleyDB.factory(dir, createIfNecessary = create)
    implicit val system: S = Confluent(fact)
    val (access, (_cursor, aural)) = system.cursorRoot[Data, (Cursor[S], AuralSystem[S])](implicit tx =>
      new Data {
        val elements  = Folder[S](tx)
        val cursor    = tx.newCursor()
      }
    )(implicit tx => data => {
      implicit val cursor = data.cursor
      cursor -> AuralSystem[S]
    })

    implicit val cursor = _cursor
    implicit val cfTpe  = reflect.runtime.universe.typeOf[Cf]
    new Impl(dir, system, access, aural)
  }

  private abstract class Data {
    def elements: Folder[S]
    def cursor: confluent.Cursor[S]

    final def write(out: DataOutput) {
      elements.write(out)
      cursor  .write(out)
    }

    final def dispose()(implicit tx: S#Tx) {
      elements.dispose()
      cursor  .dispose()
    }
  }

  private final class Impl(val folder: File, val system: S, access: S#Entry[Data], val aural: AuralSystem[S])
                          (implicit val cursor: Cursor[S], val systemType: reflect.runtime.universe.TypeTag[S])
    extends Document[S] {
    override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

    def elements(implicit tx: S#Tx): Folder[S] = access().elements

    type I = system.I
    val inMemory = (tx: S#Tx) => Confluent.inMemory(tx)
  }
}