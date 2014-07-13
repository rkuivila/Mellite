package de.sciss.mellite
package impl

import de.sciss.lucre.{event => evt}
import evt.{EventLike, Sys}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.IDPeek
import de.sciss.serial.DataOutput

import scala.collection.mutable
import scala.concurrent.{Promise, Future}
import scala.concurrent.stm.{InTxn, TMap, TSet, TxnLocal}

object ActionImpl {
  private val count = TxnLocal(0)

  private val COOKIE  = 0x61637400   // "act\0"

  def compile[S <: Sys[S]](source: Code.Action)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Future[Action[S]] = {
    val id      = tx.newID()
    val cnt     = count.getAndTransform(_ + 1)(tx.peer)
    val name    = s"Action${IDPeek(id)}_$cnt"
    val p       = Promise[Action[S]]()
    val system  = tx.system
    tx.afterCommit(performCompile(p, name, source, system))
    p.future
  }

  private def performCompile[S <: Sys[S]](p: Promise[Action[S]], name: String,
                                          source: Code.Action, system: S): Unit = {
    val jarFut = source.compileToFunction(name)
    val actFut = jarFut.map { jar =>
      sync.synchronized {
        val cl = clMap.getOrElseUpdate(system, new MemoryClassLoader)
        new ConstImpl[S](name, jar, system, cl)
      }
    }
    p.completeWith(actFut)
  }

  private val sync = new AnyRef

  // this is why workspace should have a general caching system
  private val clMap = new mutable.WeakHashMap[Sys[_], MemoryClassLoader]

  // is this assumption correct: since we hold both a `system` and `cl` instance, `cl` will
  // remain valid until the `ConstImpl` itself is GC'ed?
  private final class ConstImpl[S <: Sys[S]](name: String, jar: Array[Byte], val system: S, cl: MemoryClassLoader)
    extends Action[S] {

    def execute()(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      cl.add(name, jar)
      val clazz = Class.forName(name, true, cl)
      //  println("Instantiating...")
      val fun = clazz.newInstance().asInstanceOf[() => Unit]
      fun()
    }

    def write(out: DataOutput): Unit = {
      out.writeInt(COOKIE)
      out.writeUTF(name)
      out.writeInt(jar.length)
      out.write(jar)
    }

    def dispose()(implicit tx: S#Tx): Unit = ()

    def changed: EventLike[S, Unit] = evt.Dummy[S, Unit]
  }

  private final class MemoryClassLoader extends ClassLoader {
    // private var map: Map[String, Array[Byte]] = Map.empty
    private val setAdded    = TSet.empty[String]
    private val mapClasses  = TMap.empty[String, Array[Byte]]

    def add(name: String, jar: Array[Byte])(implicit tx: InTxn): Unit =
      if (setAdded.add(name)) {
        val entries = JarUtil.unpack(jar)
        mapClasses ++= entries
      }

    override protected def findClass(name: String): Class[_] =
      mapClasses.single.get(name).map { bytes =>
        println(s"defineClass($name, ...)")
        defineClass(name, bytes, 0, bytes.length)

      } .getOrElse(super.findClass(name)) // throws exception
  }
}
