/*
 *  Element.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import de.sciss.lucre.{stm, event => evt}
import de.sciss.synth.proc.{ProcGroup => _ProcGroup, Artifact => _Artifact, Grapheme}
import de.sciss.lucre.expr.Expr
import stm.{Disposable, Mutable}
import annotation.switch
import de.sciss.{serial, mellite}
import evt.{EventLike, EventLikeSerializer}
import collection.immutable.{IndexedSeq => Vec}
import language.higherKinds
import de.sciss.serial.{DataOutput, DataInput, Writable}
import de.sciss.{model => m}
import de.sciss.lucre.synth.{InMemory, Sys}
import de.sciss.lucre.expr.{Double => DoubleEx, Int => IntEx, String => StringEx}

object Element {
  import scala.{Int => _Int, Double => _Double}
  import java.lang.{String => _String}
  import mellite.{Folder => _Folder, Recursion => _Recursion, Code => _Code}

  type Name[S <: Sys[S]] = Expr.Var[S, _String]

  // ----------------- Updates -----------------

  final case class Update [S <: Sys[S]](element: Element[S], changes: Vec[Change[S]])

  sealed trait Change[S <: Sys[S]]
  final case class Renamed[S <: Sys[S]](change: m.Change[_String]) extends Change[S]
  final case class Entity [S <: Sys[S]](change: Any) extends Change[S]

  sealed trait Companion[E[S <: Sys[S]] <: Writable ] {
    final private[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                    (implicit tx: S#Tx): E[S] with evt.Node[S] = {
      val name = StringEx.readVar(in, access)
      read(in, access, targets, name)
    }

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): E[S] with evt.Node[S]
    protected def typeID: _Int

    implicit final def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, E[S]] = anySer.asInstanceOf[Serializer[S]]

    private val anySer = new Serializer[InMemory]
    private final class Serializer[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, E[S]] {
      def write(v: E[S], out: DataOutput): Unit = v.write(out)

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): E[S] = {
        val targets = evt.Targets.read[S](in, access)
        val cookie  = in.readInt()
        require(cookie == typeID, s"Cookie $cookie does not match expected value $typeID")
        readIdentified(in, access, targets)
      }
    }
  }

  // ----------------- Int -----------------

  object Int extends Companion[Int] {
    protected[Element] final val typeID = IntEx.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Int[S] with evt.Node[S] = {
      val entity = IntEx.readVar(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = {
      new Impl(evt.Targets[S], mkName(name), IntEx.newVar(init))
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr.Var[S, _Int])
      extends ExprImpl[S, _Int] with Int[S] {
      def prefix = "Int"
      def typeID = Int.typeID
    }
  }
  sealed trait Int[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _Int] }

  // ----------------- Double -----------------

  object Double extends Companion[Double] {
    protected[Element] final val typeID = DoubleEx.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Double[S] with evt.Node[S] = {
      val entity = DoubleEx.readVar(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = {
      new Impl(evt.Targets[S], mkName(name), DoubleEx.newVar(init))
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr.Var[S, _Double])
      extends ExprImpl[S, _Double] with Double[S] {
      def typeID = Double.typeID
      def prefix = "Double"
    }
  }
  sealed trait Double[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _Double] }

  // ----------------- String -----------------

  object String extends Companion[String] {
    protected[Element] final val typeID = StringEx.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): String[S] with evt.Node[S] = {
      val entity = StringEx.readVar(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _String])(implicit tx: S#Tx): String[S] = {
      new Impl(evt.Targets[S], mkName(name), StringEx.newVar(init))
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr.Var[S, _String])
      extends ExprImpl[S, _String] with String[S] {
      def typeID = String.typeID
      def prefix = "String"
    }
  }
  sealed trait String[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _String] }

  // ----------------- (Element) Folder -----------------

  object Folder extends Companion[Folder] {
    protected[Element] final val typeID = 0x10000

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Folder[S] with evt.Node[S] = {
      val entity = _Folder.read(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: _Folder[S])(implicit tx: S#Tx): Folder[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: _Folder[S])
      extends Element.ActiveImpl[S] with Folder[S] {
      self =>

      def typeID = Folder.typeID
      def prefix = "Folder"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait Folder[S <: Sys[S]] extends Element[S] { type A = _Folder[S] }

  // ----------------- ProcGroup -----------------

  object ProcGroup extends Companion[ProcGroup] {
    protected[Element] final val typeID = 0x10001

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): ProcGroup[S] with evt.Node[S] = {
      val entity = _ProcGroup.read(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: _ProcGroup[S])(implicit tx: S#Tx): ProcGroup[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: _ProcGroup[S])
      extends Element.ActiveImpl[S] with ProcGroup[S] {
      self =>

      def typeID = ProcGroup.typeID
      def prefix = "ProcGroup"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait ProcGroup[S <: Sys[S]] extends Element[S] { type A = _ProcGroup[S] }

  // ----------------- AudioGrapheme -----------------

  object AudioGrapheme extends Companion[AudioGrapheme] {
    protected[Element] final val typeID = 0x10002 // Grapheme.Elem.Audio.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): AudioGrapheme[S] with evt.Node[S] = {
      val entity = Grapheme.Elem.Audio.read(in, access) match {
        case a: Grapheme.Elem.Audio[S] => a
        case other => sys.error(s"Expected a Grapheme.Elem.Audio, but found $other")  // XXX TODO
      }
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): AudioGrapheme[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S],
                                          val entity: Grapheme.Elem.Audio[S]) // Expr[S, Grapheme.Value.Audio]
      extends Element.ActiveImpl[S] with AudioGrapheme[S] {
      self =>

      def typeID = AudioGrapheme.typeID
      def prefix = "AudioGrapheme"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait AudioGrapheme[S <: Sys[S]] extends Element[S] {
    type A = Grapheme.Elem.Audio[S] // Expr[S, Grapheme.Value.Audio]
  }

  // ----------------- ArtifactLocation -----------------

  object ArtifactLocation extends Companion[ArtifactLocation] {
    protected[Element] final val typeID = 0x10003

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): ArtifactLocation[S] with evt.Node[S] = {
      val entity = _Artifact.Location.read(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: _Artifact.Location[S])(implicit tx: S#Tx): ArtifactLocation[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: _Artifact.Location[S])
      extends Element.ActiveImpl[S] with ArtifactLocation[S] {
      self =>

      def typeID = ArtifactLocation.typeID
      def prefix = "ArtifactLocation"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait ArtifactLocation[S <: Sys[S]] extends Element[S] { type A = _Artifact.Location[S] }

  // ----------------- Recursion -----------------

  object Recursion extends Companion[Recursion] {
    protected[Element] final val typeID = 0x20000

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Recursion[S] with evt.Node[S] = {
      val entity = _Recursion.read(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: _Recursion[S])(implicit tx: S#Tx): Recursion[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: _Recursion[S])
      extends Element.ActiveImpl[S] with Recursion[S] {
      self =>

      def typeID = Recursion.typeID
      def prefix = "Recursion"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait Recursion[S <: Sys[S]] extends Element[S] { type A = _Recursion[S] }

  object Code extends Companion[Code] {
    protected[Element] final val typeID = Codes.typeID // 0x20001

    //    implicit def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, Code[S]] = anySer.asInstanceOf[Ser[S]]
    //
    //    private val anySer = new Ser[InMemory]
    //
    //    private final class Ser[S <: Sys[S]] extends evt.NodeSerializer[S, Code[S]] {
    //      def read(in: DataInput, access: S#Acc, targets: Targets[S])(implicit tx: S#Tx): Code[S] with evt.Node[S] = {
    //        val typeID = in.readInt()
    //        require(typeID == Code.typeID, s"Unexpected typeID $typeID (should be ${Code.typeID})")
    //        readIdentified(in, access, targets)
    //      }
    //    }

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Code[S] with evt.Node[S] = {
      val entity = Codes.read(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Code])(implicit tx: S#Tx): Code[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr[S, _Code])
      extends Element.ActiveImpl[S] with Code[S] {
      self =>

      def typeID = Code.typeID
      def prefix = "Code"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait Code[S <: Sys[S]] extends Element[S] { type A = Expr[S, _Code] }

  // ----------------- Serializer -----------------

  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Element[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends EventLikeSerializer[S, Element[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): Element[S] = {
      sys.error("No passive elements known")
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Element[S] with evt.Node[S] = {
      val typeID = in.readInt()
      (typeID: @switch) match {
        case Int             .typeID => Int             .readIdentified(in, access, targets)
        case Double          .typeID => Double          .readIdentified(in, access, targets)
        case String          .typeID => String          .readIdentified(in, access, targets)
        case Folder          .typeID => Folder          .readIdentified(in, access, targets)
        case ProcGroup       .typeID => ProcGroup       .readIdentified(in, access, targets)
        case AudioGrapheme   .typeID => AudioGrapheme   .readIdentified(in, access, targets)
        case ArtifactLocation.typeID => ArtifactLocation.readIdentified(in, access, targets)
        case Recursion       .typeID => Recursion       .readIdentified(in, access, targets)
        case Code            .typeID => Code            .readIdentified(in, access, targets)
        case _                       => sys.error(s"Unexpected element type cookie $typeID")
      }
    }

    //    def write(elem: Element[S], out: DataOutput): Unit = elem.write(out)
  }

  private def mkName[S <: Sys[S]](name: _String)(implicit tx: S#Tx): Name[S] =
    StringEx.newVar[S](StringEx.newConst(name))

  //  // A[ ~ <: Sys[ ~ ] forSome { type ~ }]
  //  private def mkExpr[S <: Sys[S], A1](biType: BiType[A1], init: Expr[S, A1], name: Option[String])
  //                                     (implicit tx: S#Tx) : (_Int, Expr.Var[S, Option[String]], Expr.Var[S, A1]) = {
  //    val expr = biType.newVar[S](init)
  //    (biType.typeID, mkName(name), expr)
  //  }

  //  private def mkImpl[S <: Sys[S], A1](typeID: Int, name: Option[String],
  //                                      elem: A1 with Writable with Disposable[S#Tx])
  //                                     (implicit tx: S#Tx): Element[S] { type A = A1 } = {
  //    val id      = tx.newID()
  //    val nameEx  = mkName(name)
  //    new ActiveImpl(id, nameEx, typeID, elem)
  //  }

  private sealed trait ExprImpl[S <: Sys[S], A1] extends ActiveImpl[S] {
    self =>
    type A <: Expr[S, A1]
    final protected def entityEvent = entity.changed
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Element[S] with evt.Node[S] {
    self =>

    type A <: Writable with Disposable[S#Tx]

    protected def typeID: _Int

    final protected def writeData(out: DataOutput): Unit = {
      out.writeInt(typeID)
      name.write(out)
      entity.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx): Unit = {
      name.dispose()
      entity.dispose()
    }

    protected def prefix: _String

    override def toString() = s"Element.${prefix}$id"

    // ---- events ----

    final protected def reader: evt.Reader[S, Element[S]] = serializer

    final protected def foldUpdate(sum: Option[Update[S]], inc: Update[S])(implicit tx: S#Tx): Option[Update[S]] =
      sum match {
        case Some(prev) => Some(prev.copy(changes = prev.changes ++ inc.changes))
        case _          => Some(inc)
      }

    trait EventImpl
      extends evt.impl.EventImpl[S, Update[S], Element[S]] with evt.InvariantEvent[S, Update[S], Element[S]] {

      final protected def reader: evt.Reader[S, Element[S]] = self.reader
      final def node: Element[S] with evt.Node[S] = self
    }

    protected object NameChange extends EventImpl {
      final val slot = 0
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        pull(name.changed).map(ch => Update(self, Vec(Renamed(ch))))
      }

      def connect   ()(implicit tx: S#Tx): Unit = name.changed ---> this
      def disconnect()(implicit tx: S#Tx): Unit = name.changed -/-> this
    }
  }

  private sealed trait PassiveImpl[S <: Sys[S]]
    extends Impl[S] {

    def changed: EventLike[S, Element.Update[S]] = NameChange
  }

  private sealed trait ActiveImpl[S <: Sys[S]]
    extends Impl[S] with evt.impl.Reducer[S, Update[S], Update[S], Element[S]] {
    self =>

    // ---- events ----

    protected def entityEvent: evt.EventLike[S, Any]

    final protected def events = Vec(NameChange, EntityChange)
    final protected def changedSlot = 2

    private object EntityChange extends EventImpl {
      final val slot = 1
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        pull(entityEvent).map(ch => Update(self, Vec(Entity(ch))))
      }

      def connect   ()(implicit tx: S#Tx): Unit = entityEvent ---> this
      def disconnect()(implicit tx: S#Tx): Unit = entityEvent -/-> this
    }
  }
}
/** Elements are what a document is made from. They comprise heterogeneous objects from expressions (integer, double,
  * string etc. expressions), processes and groups of processes, as well as `Element.Group` which is a container
  * for nested elements.
  */
sealed trait Element[S <: Sys[S]] extends evt.Publisher[S, Element.Update[S]] with evt.Node[S] {
  type A

  /** An element always has a variable name attached to it. */
  def name: Expr.Var[S, String]
  /** The actual object wrapped by the element. */
  def entity: A

  /** An event for tracking element changes, which can be renaming
    * the element or forwarding changes from the underlying entity.
    */
  def changed: EventLike[S, Element.Update[S]]
}