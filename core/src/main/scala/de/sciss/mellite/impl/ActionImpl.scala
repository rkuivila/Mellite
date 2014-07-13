/*
 *  ActionImpl.scala
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

import de.sciss.lucre.{event => evt}
import de.sciss.synth.proc
import de.sciss.synth.proc.Elem
import de.sciss.lucre.event.{Reader, InMemory, Node, Targets, EventLike, Sys}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.IDPeek
import de.sciss.serial.{DataInput, DataOutput}

import scala.annotation.switch
import scala.collection.mutable
import scala.concurrent.{Promise, Future}
import scala.concurrent.stm.{InTxn, TMap, TSet, TxnLocal}

object ActionImpl {
  private val count = TxnLocal(0) // to distinguish different action class-names within the same transaction

  private final val COOKIE        = 0x61637400   // "act\0"
  private final val CONST_EMPTY   = 0
  private final val CONST_FUN     = 1
  private final val CONST_VAR     = 2

  private val DEBUG = true

  // ---- creation ----

  def compile[S <: Sys[S]](source: Code.Action)
                          (implicit tx: S#Tx, cursor: stm.Cursor[S]): Future[stm.Source[S#Tx, Action[S]]] = {
    val id      = tx.newID()
    val cnt     = count.getAndTransform(_ + 1)(tx.peer)
    val name    = s"Action${IDPeek(id)}_$cnt"
    val p       = Promise[stm.Source[S#Tx, Action[S]]]()
    val system  = tx.system
    tx.afterCommit(performCompile(p, name, source, system))
    p.future
  }

  def empty[S <: Sys[S]](implicit tx: S#Tx): Action[S] = new ConstEmptyImpl[S]

  def newVar[S <: Sys[S]](init: Action[S])(implicit tx: S#Tx): Action.Var[S] = {
    val targets = evt.Targets[S]
    val peer    = tx.newVar(targets.id, init)
    new VarImpl[S](targets, peer)
  }

  // ----

  private def performCompile[S <: Sys[S]](p: Promise[stm.Source[S#Tx, Action[S]]], name: String,
                                          source: Code.Action, system: S)(implicit cursor: stm.Cursor[S]): Unit = {
    val jarFut = source.compileToFunction(name)
    val actFut = jarFut.map { jar =>
      if (DEBUG) println(s"compileToFunction completed. jar-size = ${jar.length}")
      cursor.step { implicit tx =>
        val a: Action[S] = sync.synchronized {
          val cl = clMap.getOrElseUpdate(system, {
            if (DEBUG) println("Create new class loader")
            new MemoryClassLoader
          })
          new ConstFunImpl(name, jar, system, cl)
        }
        tx.newHandle(a)
      }
    }
    p.completeWith(actFut)
  }

  // ---- serialization ----

  def serializer[S <: Sys[S]]: evt.EventLikeSerializer[S, Action[S]] = anySer.asInstanceOf[Ser[S]]

  private val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends evt.EventLikeSerializer[S, Action[S]] {
    private def readCookieAndTpe(in: DataInput): Byte = {
      val cookie = in.readInt()
      if (cookie != COOKIE)
        sys.error(s"Unexpected cookie (found ${cookie.toHexString}, expected ${COOKIE.toHexString})")
      in.readByte()
    }

    def readConstant(in: DataInput)(implicit tx: S#Tx): Action[S] =
      (readCookieAndTpe(in): @switch) match {
        case CONST_FUN    =>
          val name    = in.readUTF()
          val jarSize = in.readInt()
          val jar     = new Array[Byte](jarSize)
          in.readFully(jar)
          val system  = tx.system
          sync.synchronized {
            val cl = clMap.getOrElseUpdate(system, {
              if (DEBUG) println("Create new class loader")
              new MemoryClassLoader
            })
            new ConstFunImpl[S](name, jar, system, cl)
          }

        case CONST_EMPTY  => new ConstEmptyImpl[S]

        case other => sys.error(s"Unexpected action cookie $other")
      }

    def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Action[S] with Node[S] =
      (readCookieAndTpe(in): @switch) match {
        case CONST_VAR =>
          val peer = tx.readVar[Action[S]](targets.id, in)
          new VarImpl[S](targets, peer)

        case other => sys.error(s"Unexpected action cookie $other")
      }
  }

  // ---- constant implementation ----

  private val sync = new AnyRef

  // this is why workspace should have a general caching system
  private val clMap = new mutable.WeakHashMap[Sys[_], MemoryClassLoader]

  private sealed trait ConstImpl[S <: Sys[S]] extends Action[S] with evt.impl.Constant {
    def dispose()(implicit tx: S#Tx): Unit = ()

    def changed: EventLike[S, Unit] = evt.Dummy[S, Unit]
  }

  // is this assumption correct: since we hold both a `system` and `cl` instance, `cl` will
  // remain valid until the `ConstImpl` itself is GC'ed?
  private final class ConstFunImpl[S <: Sys[S]](val name: String, jar: Array[Byte],
                                                val system: S, cl: MemoryClassLoader)
    extends ConstImpl[S] {

    def execute()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      cl.add(name, jar)
      val fullName  = s"${CodeImpl.UserPackage}.$name"
      val clazz     = Class.forName(fullName, true, cl)
      //  println("Instantiating...")
      val fun = clazz.newInstance().asInstanceOf[() => Unit]
      fun()
    }

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_FUN)
      out.writeUTF(name)
      out.writeInt(jar.length)
      out.write(jar)
    }

    override def hashCode(): Int = name.hashCode

    override def equals(that: Any): Boolean = that match {
      case cf: ConstFunImpl[_] => cf.name == name
      case _ => super.equals(that)
    }
  }

  private final class ConstEmptyImpl[S <: Sys[S]] extends ConstImpl[S] {
    def execute()(implicit tx: S#Tx): Unit = ()

    override def equals(that: Any): Boolean = that match {
      case e: ConstEmptyImpl[_] => true
      case _ => super.equals(that)
    }

    override def hashCode(): Int = 0

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_EMPTY)
    }
  }

  private final class VarImpl[S <: Sys[S]](protected val targets: evt.Targets[S], peer: S#Var[Action[S]])
    extends Action.Var[S] with evt.impl.SingleGenerator[S, Unit, Action[S]] {

    def apply()(implicit tx: S#Tx): Action[S] = peer()

    def update(value: Action[S])(implicit tx: S#Tx): Unit = {
      val old = peer()
      peer()  = value
      if (old != value) fire(())
    }

    def execute()(implicit tx: S#Tx): Unit = peer().execute()

    protected def disposeData()(implicit tx: S#Tx): Unit = peer.dispose()

    protected def writeData(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeByte(CONST_VAR)
      peer.write(out)
    }

    protected def reader: Reader[S, Action[S]] = ActionImpl.serializer
  }

  // ---- class loader ----

  private final class MemoryClassLoader extends ClassLoader {
    // private var map: Map[String, Array[Byte]] = Map.empty
    private val setAdded    = TSet.empty[String]
    private val mapClasses  = TMap.empty[String, Array[Byte]]

    def add(name: String, jar: Array[Byte])(implicit tx: InTxn): Unit = {
      val isNew = setAdded.add(name)
      if (DEBUG) println(s"Class loader add '$name' - isNew? $isNew")
      if (isNew) {
        val entries = JarUtil.unpack(jar)
        if (DEBUG) {
          entries.foreach { case (n, _) =>
            println(s"...'$n'")
          }
        }
        mapClasses ++= entries
      }
    }

    override protected def findClass(name: String): Class[_] =
      mapClasses.single.get(name).map { bytes =>
        if (DEBUG) println(s"Class loader: defineClass '$name'")
        defineClass(name, bytes, 0, bytes.length)

      } .getOrElse {
        if (DEBUG) println(s"Class loader: not found '$name' - calling super")
        super.findClass(name) // throws exception
      }
  }


  // ---- elem ----

  object ElemImpl extends proc.impl.ElemImpl.Companion[Action.Elem] {
    def typeID = Action.typeID

    Elem.registerExtension(this)

    def apply[S <: Sys[S]](peer: Action[S])(implicit tx: S#Tx): Action.Elem[S] = {
      val targets = evt.Targets[S]
      new ActiveImpl[S](targets, peer)
    }

    def read[S <: Sys[S]](in: DataInput, access: S#Acc)(implicit tx: S#Tx): Action.Elem[S] =
      serializer[S].read(in, access)

    // ---- Elem.Extension ----

    /** Read identified active element */
    def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                   (implicit tx: S#Tx): Action.Elem[S] with evt.Node[S] = {
      val peer = ActionImpl.serializer.read(in, access)
      new ActiveImpl[S](targets, peer)
    }

    /** Read identified constant element */
    def readIdentifiedConstant[S <: Sys[S]](in: DataInput)(implicit tx: S#Tx): Action.Elem[S] =
      sys.error("Constant Action not supported")

    // ---- implementation ----

    private sealed trait Impl {
      def typeID = Action.typeID
      def prefix = "Action"
    }

    private final class ActiveImpl[S <: Sys[S]](protected val targets: evt.Targets[S],
                                                val peer: Action[S])
      extends Action.Elem[S]
      with proc.impl.ElemImpl.Active[S] with Impl {

      override def toString() = s"$prefix.Elem$id"

      def mkCopy()(implicit tx: S#Tx): Action.Elem[S] = Action.Elem(peer)
    }

    private final class PassiveImpl[S <: Sys[S]](val peer: Action[S])
      extends Action.Elem[S]
      with proc.impl.ElemImpl.Passive[S] with Impl {

      override def toString() = s"$prefix.Elem($peer)"
    }
  }
}
