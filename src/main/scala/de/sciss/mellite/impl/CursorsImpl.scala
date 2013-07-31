package de.sciss
package mellite
package impl

import lucre.{event => evt, stm, data, expr, confluent}
import de.sciss.serial.{DataInput, DataOutput}
import scala.annotation.switch
import confluent.reactive.{ConfluentReactiveLike => KSys}
import evt.{DurableLike => DSys}
import de.sciss.lucre.expr.Expr
import de.sciss.synth.expr.{ExprImplicits, Strings}

object CursorsImpl {
  private final val COOKIE = 0x43737273 // "Csrs"

  def apply[S <: KSys[S], D1 <: DSys[D1]](seminal: S#Acc)
                                         (implicit tx: D1#Tx, system: S { type D = D1 }): Cursors[S, D1] = {
    val imp     = ExprImplicits[D1]
    import imp._
    val targets = evt.Targets[D1]
    val cursor  = confluent.Cursor[S, D1](seminal)
    val name    = Strings.newVar("branch")
    val list    = expr.LinkedList.Modifiable[D1, Cursors[S, D1], Cursors.Update[S, D1]](_.changed)
    log(s"Cursors.apply targets = $targets, list = $list")
    new Impl(targets, seminal, cursor, name, list)
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
      val name    = Strings.readVar[D1](in, access)
      val list    = expr.LinkedList.Modifiable.read[D1, Cursors[S, D1], Cursors.Update[S, D1]](_.changed)(in, access)
      log(s"Cursors.read targets = $targets, list = $list")
      new Impl(targets, seminal, cursor, name, list)
    }
  }

  private final class Impl[S <: KSys[S], D1 <: DSys[D1]](
      protected val targets: evt.Targets[D1], val seminal: S#Acc with serial.Writable,
      val cursor: confluent.Cursor[S, D1] with stm.Disposable[D1#Tx] with serial.Writable,
      nameVar: Expr.Var[D1, String],
      list: expr.LinkedList.Modifiable[D1, Cursors[S, D1], Cursors.Update[S, D1]]
    )(implicit tx: D1#Tx, system: S { type D = D1 })
    extends Cursors[S, D1] with evt.Node[D1] {
    impl =>
    
    override def toString() = "Cursors" + id

    def name(implicit tx: D1#Tx): Expr[D1, String] = nameVar()
    def name_=(value: Expr[D1, String])(implicit tx: D1#Tx) { nameVar() = value }

    def descendants(implicit tx: D1#Tx): data.Iterator[D1#Tx, Cursors[S, D1]] = list.iterator

    def addChild(seminal: S#Acc)(implicit tx: D1#Tx): Cursors[S, D1] = {
      val child = CursorsImpl[S, D1](seminal)
      log(s"$this.addChild($child)")
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
      nameVar.write(out)
      list   .write(out)
    }

    protected def disposeData()(implicit tx: D1#Tx) {
      cursor .dispose()
      nameVar.dispose()
      list   .dispose()
    }

    //    private object GeneratorEvent
    //      extends evt.impl.TriggerImpl[D1, Cursors.Change[S, D1], Cursors[S, D1]]
    //      with evt.impl.EventImpl     [D1, Cursors.Change[S, D1], Cursors[S, D1]]
    //      with evt.InvariantEvent     [D1, Cursors.Change[S, D1], Cursors[S, D1]]
    //      with evt.impl.Root          [D1, Cursors.Change[S, D1]] {
    //
    //      protected def reader: evt.Reader[D1, Cursors[S, D1]] = serializer
    //
    //      override def toString() = node.toString + ".GeneratorEvent"
    //      final val slot = 0
    //      def node = impl
    //    }

    object changed
      extends evt.impl.EventImpl[D1, Cursors.Update[S, D1], Cursors[S, D1]]
      with evt.InvariantEvent   [D1, Cursors.Update[S, D1], Cursors[S, D1]] {

      protected def reader: evt.Reader[D1, Cursors[S, D1]] = serializer

      override def toString() = node.toString + ".changed"
      final val slot = 0

      def node = impl

      def connect()(implicit tx: D1#Tx) {
        // log(s"$this.connect")
        // GeneratorEvent ---> this
        list.changed    ---> this
        nameVar.changed ---> this
      }

      def disconnect()(implicit tx: D1#Tx) {
        // log(s"$this.disconnect()")
        // GeneratorEvent -/-> this
        list.changed    -/-> this
        nameVar.changed -/-> this
      }

      def pullUpdate(pull: evt.Pull[D1])(implicit tx: D1#Tx): Option[Cursors.Update[S, D1]] = {
        val listEvt = list   .changed
        val nameEvt = nameVar.changed
        // val genOpt  = if (pull.contains(GeneratorEvent)) pull(GeneratorEvent) else None

        val nameOpt = if (pull.contains(nameEvt)) pull(nameEvt) else None
        Thread.sleep(50)
        val listOpt = if (pull.contains(listEvt)) pull(listEvt) else None

        // println(s"---- enter pull : list = $listOpt")

        // val flat1   = genOpt.toIndexedSeq
        val flat1   = nameOpt.map(Cursors.Renamed[S, D1]).toIndexedSeq
        val changes = listOpt match {
          case Some(listUpd) =>
            val childUpdates = listUpd.changes.collect {
              case expr.LinkedList.Element(child, childUpd) => Cursors.ChildUpdate (childUpd)
              case expr.LinkedList.Added  (idx, child)      => Cursors.ChildAdded  (idx, child)
              case expr.LinkedList.Removed(idx, child)      => Cursors.ChildRemoved(idx, child)
            }
            flat1 ++ childUpdates

          case _ => flat1
        }

        // println(s"---- exit pull : changes = $changes")

        if (changes.isEmpty) None else Some(Cursors.Update(impl, changes))
      }
    }

    def select(slot: Int /*, invariant: Boolean */): evt.Event[D1, Any, Any] = (slot: @switch) match {
      // case GeneratorEvent.slot  => GeneratorEvent
      case changed.slot         => changed
    }
  }
}