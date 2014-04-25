/*
 *  DocumentImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package impl

import java.io.{FileOutputStream, IOException, FileNotFoundException}
import de.sciss.file._
import de.sciss.lucre.{confluent, stm}
import stm.store.BerkeleyDB
import de.sciss.synth.proc.{FolderElem, Obj, Folder, ExprImplicits, Confluent}
import de.sciss.serial.{DataInput, Serializer, DataOutput}
import scala.collection.immutable.{IndexedSeq => Vec}

object DocumentImpl {
  private type S = Cf

  private implicit object serializer extends Serializer[S#Tx, S#Acc, Data] {
    def write(data: Data, out: DataOutput): Unit =
      data.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data = {
      val cookie = in.readLong()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (should be $COOKIE)")
      new Data {
        val root = Folder.read[S](in, access)
      // val cursor    = ... // tx.readCursor(in, access)
      }
    }
  }

  def read(dir: File): ConfluentDocument = {
    if (!dir.isDirectory) throw new FileNotFoundException(s"Document ${dir.path} does not exist")
    apply(dir, create = false)
  }

  def empty(dir: File): ConfluentDocument = {
    if (dir.exists()) throw new IOException(s"Document ${dir.path} already exists")
    apply(dir, create = true)
  }

  private def apply(dir: File, create: Boolean): ConfluentDocument = {
    type S    = Cf
    val fact  = BerkeleyDB.factory(dir, createIfNecessary = create)
    new FileOutputStream(dir / "open").close()
    implicit val system: S = Confluent(fact)

    val (access, cursors) = system.rootWithDurable[Data, Cursors[S, S#D]] { implicit tx =>
      val data: Data = new Data {
        val root = Folder[S](tx)
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
    def root: Folder[S]
    // def cursor: confluent.Cursor[S, S#D]

    final def write(out: DataOutput): Unit = {
      out.writeLong(COOKIE)
      root.write(out)
      // cursor  .write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit =
      root.dispose()

    override def toString = s"Data ($root)"
  }

  private final class Impl(val folder: File, val system: S, access: stm.Source[S#Tx, Data],
                           val cursors: Cursors[S, S#D])
                          (implicit val systemType: reflect.runtime.universe.TypeTag[S])
    extends ConfluentDocument {
    override def toString = "Document<" + folder.getName + ">" // + hashCode().toHexString

    def root(implicit tx: S#Tx): Folder[S] = access().root

    type I = system.I
    val inMemoryBridge = (tx: S#Tx) => Confluent.inMemory(tx)
    def inMemoryCursor: stm.Cursor[I] = system.inMemory

    def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A] = {
      var b   = Vec.newBuilder[A]
      val fun = pf.lift

      def loop(f: Folder[S]): Unit =
        f.iterator.foreach { obj =>
          fun(obj).foreach(b += _)
          obj.elem match {
            case ef: FolderElem[S] => loop(ef.peer)
            case _ =>
          }
        }

      loop(root)
      b.result()
    }
  }
}