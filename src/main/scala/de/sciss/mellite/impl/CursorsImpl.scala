package de.sciss
package mellite
package impl

import lucre.{event => evt, stm, data, expr, confluent}
import de.sciss.serial.{DataInput, DataOutput}
import scala.annotation.switch
import confluent.reactive.{ConfluentReactiveLike => KSys}
import evt.{DurableLike => DSys}

object CursorsImpl {
  private final val COOKIE = 0x43737273 // "Csrs"

  def apply[S <: KSys[S], D1 <: DSys[D1]](seminal: S#Acc)
                                         (implicit tx: D1#Tx, system: S { type D = D1 }): Cursors[S, D1] = {
    val targets = evt.Targets[D1]
    val cursor  = confluent.Cursor[S, D1](seminal)
    val list    = expr.LinkedList.Modifiable[D1, Cursors[S, D1], Cursors.Update[S, D1]](_.changed)
    new Impl(targets, seminal, cursor, list)
  }

  // private final class CursorImpl
  
  //   serial.Serializer[D#Tx, D#Acc, Cursors[S, D]]

  implicit def serializer[S <: KSys[S], D1 <: DSys[D1]](implicit system: S { type D = D1 }):
    serial.Serializer[D1#Tx, D1#Acc, Cursors[S, D1]] with evt.Reader[D1, Cursors[S, D1]] = new Ser[S, D1]

  private final class Ser[S <: KSys[S], D1 <: DSys[D1]](implicit system: S { type D = D1 })
    extends evt.NodeSerializer[D1, Cursors[S, D1]] {

    def read(in: DataInput, access: D1#Acc, targets: evt.Targets[D1])(implicit tx: D1#Tx): Cursors[S, D1] with evt.Node[D1] = {
      val cookie  = in.readInt()
      require(cookie == COOKIE, s"Unexpected $cookie (should be $COOKIE)")
      val seminal: S#Acc = system.readPath(in)
      val cursor  = confluent.Cursor.read[S, D1](in)
      val list    = expr.LinkedList.Modifiable.read[D1, Cursors[S, D1], Cursors.Update[S, D1]](_.changed)(in, access)
      new Impl(targets, seminal, cursor, list)
    }
  }

  private final class Impl[S <: KSys[S], D1 <: DSys[D1]](
      protected val targets: evt.Targets[D1], val seminal: S#Acc with serial.Writable,
      val cursor: stm.Cursor[S] with stm.Disposable[D1#Tx] with serial.Writable,
      list: expr.LinkedList.Modifiable[D1, Cursors[S, D1], Cursors.Update[S, D1]]
    )(implicit tx: D1#Tx, system: S { type D = D1 })
    extends Cursors[S, D1] with evt.Node[D1] {
    impl =>
    
    override def toString() = "Cursors" + id

    def descendants(implicit tx: D1#Tx): data.Iterator[D1#Tx, Cursors[S, D1]] = list.iterator

    def addChild(seminal: S#Acc)(implicit tx: D1#Tx): Cursors[S, D1] = {
      val child = CursorsImpl[S, D1](seminal)
      list.addLast(child)
      child
    }

    def removeChild(child: Cursors[S, D1])(implicit tx: D1#Tx) {
      if (!list.remove(child)) println(s"WARNING: Cursor $child was not a child of $impl")
    }

    protected def writeData(out: DataOutput) {
      out.writeInt(COOKIE)
      seminal.write(out)
      cursor .write(out)
      list   .write(out)
    }

    protected def disposeData()(implicit tx: D1#Tx) {
      cursor.dispose()
      list  .dispose()
    }

    private object GeneratorEvent
      extends evt.impl.TriggerImpl[D1, Cursors.Change[S, D1], Cursors[S, D1]]
      with evt.impl.EventImpl     [D1, Cursors.Change[S, D1], Cursors[S, D1]]
      with evt.InvariantEvent     [D1, Cursors.Change[S, D1], Cursors[S, D1]]
      with evt.impl.Root          [D1, Cursors.Change[S, D1]] {

      protected def reader: evt.Reader[D1, Cursors[S, D1]] = serializer

      override def toString() = node.toString + ".GeneratorEvent"
      final val slot = 0
      def node = impl
    }

    object changed
      extends evt.impl.EventImpl[D1, Cursors.Update[S, D1], Cursors[S, D1]]
      with evt.InvariantEvent   [D1, Cursors.Update[S, D1], Cursors[S, D1]] {

      protected def reader: evt.Reader[D1, Cursors[S, D1]] = serializer

      override def toString() = node.toString + ".changed"
      final val slot = 2

      def node = impl

      def connect()(implicit tx: D1#Tx) {
        // log(s"$this.connect")
        GeneratorEvent ---> this
        list.changed   ---> this
      }

      def disconnect()(implicit tx: D1#Tx) {
        // log(s"$this.disconnect()")
        GeneratorEvent -/-> this
        list.changed   -/-> this
      }

      def pullUpdate(pull: evt.Pull[D1])(implicit tx: D1#Tx): Option[Cursors.Update[S, D1]] = {
        val listEvt = list.changed
        val genOpt  = if (pull.contains(GeneratorEvent)) pull(GeneratorEvent) else None
        val listOpt = if (pull.contains(listEvt       )) pull(listEvt       ) else None

        val flat1   = genOpt.toIndexedSeq
        val changes = listOpt match {
          case Some(listUpd) =>
            val childUpdates = listUpd.changes.collect {
              case expr.LinkedList.Element(child, childUpd) => Cursors.ChildUpdate(childUpd)
            }
            flat1 ++ childUpdates

          case _ => flat1
        }

        if (changes.isEmpty) None else Some(Cursors.Update(impl, changes))
      }
    }

    def select(slot: Int, invariant: Boolean): evt.Event[D1, Any, Any] = (slot: @switch) match {
      case GeneratorEvent.slot  => GeneratorEvent
      case changed.slot         => changed
    }
  }
}