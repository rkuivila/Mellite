/*
 *  WorkspaceImpl.scala
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

import java.io.{FileInputStream, FileOutputStream, IOException, FileNotFoundException}
import de.sciss.file._
import de.sciss.lucre.{confluent, stm}
import stm.store.BerkeleyDB
import de.sciss.synth.proc.{Durable => Dur, Confluent, Obj, Folder, FolderElem, ExprImplicits}
import de.sciss.serial.{DataInput, Serializer, DataOutput}
import scala.collection.immutable.{IndexedSeq => Vec}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.synth.{Sys => SSys}
import de.sciss.lucre.stm.{TxnLike, DataStoreFactory, Disposable}
import scala.concurrent.stm.{Txn, Ref}
import java.util.Properties
import scala.language.existentials

object WorkspaceImpl {
  private final class Ser[S <: Sys[S]] extends Serializer[S#Tx, S#Acc, Data[S]] {
    def write(data: Data[S], out: DataOutput): Unit =
      data.write(out)

    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Data[S] = {
      val cookie = in.readLong()
      require(cookie == COOKIE, s"Unexpected cookie $cookie (should be $COOKIE)")
      new Data[S] {
        val root = Folder.read[S](in, access)
      }
    }
  }

  private implicit val ConfluentSer: Ser[Cf]  = new Ser[Cf]
  private implicit def EphemeralSer: Ser[Dur] = ConfluentSer.asInstanceOf[Ser[Dur]]

  private def requireExists(dir: File): Unit =
    if (!dir.isDirectory) throw new FileNotFoundException(s"Workspace ${dir.path} does not exist")

  private def requireExistsNot(dir: File): Unit =
    if (dir.exists()) throw new IOException(s"Workspace ${dir.path} already exists")

  def read(dir: File): Workspace[_] /* [~ forSome { type ~ <: SSys[~] }] */ = {
    requireExists(dir)
    val fis   = new FileInputStream(dir / "open")
    val prop  = new Properties
    prop.load(fis)
    fis.close()
    val confluent = prop.getProperty("type") match {
      case "confluent"  => true
      case "ephemeral"  => false
      case other        => sys.error(s"Invalid property 'type': $other")
    }
    val res = if (confluent) readConfluent(dir) else readEphemeral(dir)
    res // .asInstanceOf[Workspace[~ forSome { type ~ <: SSys[~] }]]
  }

  def readConfluent(dir: File): Workspace.Confluent = {
    requireExists(dir)
    applyConfluent(dir, create = false)
  }

  def emptyConfluent(dir: File): Workspace.Confluent = {
    requireExistsNot(dir)
    applyConfluent(dir, create = true)
  }

  def readEphemeral(dir: File): Workspace.Ephemeral = {
    requireExists(dir)
    applyEphemeral(dir, create = false)
  }

  def emptyEphemeral(dir: File): Workspace.Ephemeral = {
    requireExistsNot(dir)
    applyEphemeral(dir, create = true)
  }

  private def openDataStore(dir: File, create: Boolean, confluent: Boolean): DataStoreFactory[BerkeleyDB] = {
    val res   = BerkeleyDB.factory(dir, createIfNecessary = create)
    val fos   = new FileOutputStream(dir / "open")
    val prop  = new Properties()
    prop.setProperty("type", if (confluent) "confluent" else "ephemeral")
    prop.store(fos, "Mellite Workspace Meta-Info")
    fos.close()
    res
  }

  private def applyConfluent(dir: File, create: Boolean): Workspace.Confluent = {
    type S    = Cf
    val fact  = openDataStore(dir, create = create, confluent = true)
    implicit val system: S = Confluent(fact)

    val (access, cursors) = system.rootWithDurable[Data[S], Cursors[S, S#D]] { implicit tx =>
      val data: Data[S] = new Data[S] {
        val root = Folder[S](tx)
      }
      data

    } { implicit tx =>
      val c   = Cursors[S, S#D](confluent.Sys.Acc.root[S])
      val imp = ExprImplicits[S#D]
      import imp._
      c.name_=("master")
      c
    }

    new ConfluentImpl(dir, system, access, cursors)
  }

  private def applyEphemeral(dir: File, create: Boolean): Workspace.Ephemeral = {
    type S    = Dur
    val fact  = openDataStore(dir, create = create, confluent = false)
    implicit val system: S = Dur(fact)

    val access = system.root[Data[S]] { implicit tx =>
      val data: Data[S] = new Data[S] {
        val root = Folder[S](tx)
      }
      data
    }

    new EphemeralImpl(dir, system, access)
  }

  private final val COOKIE = 0x4D656C6C69746500L  // "Mellite\0"

  private abstract class Data[S <: Sys[S]] {
    def root: Folder[S]

    final def write(out: DataOutput): Unit = {
      out.writeLong(COOKIE)
      root.write(out)
    }

    final def dispose()(implicit tx: S#Tx): Unit =
      root.dispose()

    override def toString = s"Data ($root)"
  }

  private trait Impl[S <: Sys[S]] {
    _: Workspace[S] =>

    // ---- abstract ----

    protected def access: stm.Source[S#Tx, Data[S]]

    protected def masterCursor: stm.Cursor[S]

    // ---- implemented ----

    override def toString = s"Workspace<${folder.name}>" // + hashCode().toHexString

    private val dependents  = Ref(Vec.empty[Disposable[S#Tx]])

    final val root: stm.Source[S#Tx, Folder[S]] = stm.Source.map(access)(_.root)

    final def addDependent   (dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit =
      dependents.transform(_ :+ dep)(tx.peer)

    final def removeDependent(dep: Disposable[S#Tx])(implicit tx: TxnLike): Unit =
      dependents.transform { in =>
        val idx = in.indexOf(dep)
        require(idx >= 0, s"Dependent $dep was not registered")
        in.patch(idx, Nil, 1)
      } (tx.peer)

    final def collectObjects[A](pf: PartialFunction[Obj[S], A])(implicit tx: S#Tx): Vec[A] = {
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

      loop(root())
      b.result()
    }

    final def close(): Unit = masterCursor.step { implicit tx =>
      dispose()
    }

    final def dispose()(implicit tx: S#Tx): Unit = {
      // logInfoTx(s"Dispose workspace $name")

      // first dispose all dependents
      val dep = dependents.get(tx.peer)
      dep.foreach(_.dispose())
      dependents.update(Vec.empty)(tx.peer)

      // if the transaction is successful...
      Txn.afterCommit { _ =>
        // ...and close the database
        log(s"Closing system $system")
        system.close()
      } (tx.peer)
    }
  }

  private final class ConfluentImpl(val folder: File, val system: Cf, protected val access: stm.Source[Cf#Tx, Data[Cf]],
                                    val cursors: Cursors[Cf, Cf#D])
    extends Workspace.Confluent with Impl[Cf] {

    val systemType = implicitly[reflect.runtime.universe.TypeTag[Cf]]

    type I = system.I
    val inMemoryBridge = (tx: S#Tx) => tx.inMemory  // Confluent.inMemory(tx)
    def inMemoryCursor: stm.Cursor[I] = system.inMemory

    protected def masterCursor = cursors.cursor
  }

  private final class EphemeralImpl(val folder: File, val system: Dur,
                                    protected val access: stm.Source[Dur#Tx, Data[Dur]])
    extends Workspace.Ephemeral with Impl[Dur] {

    val systemType = implicitly[reflect.runtime.universe.TypeTag[Dur]]

    type I = system.I
    val inMemoryBridge = (tx: S#Tx) => Dur.inMemory(tx)
    def inMemoryCursor: stm.Cursor[I] = system.inMemory

    protected def masterCursor = cursor
  }
}