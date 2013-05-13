/*
 *  Element.scala
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

import de.sciss.lucre.{stm, event => evt}
import de.sciss.synth.proc.{ProcGroup => _ProcGroup, Artifact => _Artifact, Grapheme, InMemory, Sys}
import de.sciss.lucre.expr.Expr
import stm.{Disposable, Mutable}
import de.sciss.synth.expr.{Doubles, Strings, Ints}
import annotation.switch
import de.sciss.{serial, mellite}
import evt.{EventLike, EventLikeSerializer}
import collection.immutable.{IndexedSeq => IIdxSeq}
import language.higherKinds
import de.sciss.serial.{DataOutput, DataInput, Writable}

object Element {
  import scala.{Int => _Int, Double => _Double}
  import java.lang.{String => _String}
  import mellite.{Folder => _Folder}

  type Name[S <: Sys[S]] = Expr.Var[S, _String]

  // ----------------- Updates -----------------

  final case class Update [S <: Sys[S]](element: Element[S], changes: IIdxSeq[Change[S]])

  sealed trait Change[S <: Sys[S]]
  final case class Renamed[S <: Sys[S]](change: evt.Change[_String]) extends Change[S]
  final case class Entity [S <: Sys[S]](change: Any) extends Change[S]

  sealed trait Companion[E[S <: Sys[S]] <: Writable ] {
    final private[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                    (implicit tx: S#Tx): E[S] with evt.Node[S] = {
      val name = Strings.readVar(in, access)
      read(in, access, targets, name)
    }

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): E[S] with evt.Node[S]
    protected def typeID: _Int

    implicit final def serializer[S <: Sys[S]]: serial.Serializer[S#Tx, S#Acc, E[S]] = anySer.asInstanceOf[Serializer[S]]

    private val anySer = new Serializer[InMemory]
    private final class Serializer[S <: Sys[S]] extends serial.Serializer[S#Tx, S#Acc, E[S]] {
      def write(v: E[S], out: DataOutput) {
        v.write(out)
      }

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
    protected[Element] final val typeID = Ints.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Int[S] with evt.Node[S] = {
      val entity = Ints.readVar(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = {
      new Impl(evt.Targets[S], mkName(name), Ints.newVar(init))
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
    protected[Element] final val typeID = Doubles.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): Double[S] with evt.Node[S] = {
      val entity = Doubles.readVar(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = {
      new Impl(evt.Targets[S], mkName(name), Doubles.newVar(init))
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
    protected[Element] final val typeID = Strings.typeID

    protected def read[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S], name: Name[S])
                                   (implicit tx: S#Tx): String[S] with evt.Node[S] = {
      val entity = Strings.readVar(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _String])(implicit tx: S#Tx): String[S] = {
      new Impl(evt.Targets[S], mkName(name), Strings.newVar(init))
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
      val entity = Grapheme.Elem.Audio.readExpr(in, access)
      new Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Grapheme.Elem.Audio[S])(implicit tx: S#Tx): AudioGrapheme[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    private final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S],
                                          val entity: Expr[S, Grapheme.Value.Audio])
      extends Element.ActiveImpl[S] with AudioGrapheme[S] {
      self =>

      def typeID = AudioGrapheme.typeID
      def prefix = "AudioGrapheme"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait AudioGrapheme[S <: Sys[S]] extends Element[S] { type A = Expr[S, Grapheme.Value.Audio] }

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

  // ----------------- Bounce -----------------
  // a bounce is a procgroup, a selection span,
  // a gain/ normalize setting, a channel selection,
  // an artifact variable

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
        case _                       => sys.error(s"Unexpected element type cookie $typeID")
      }
    }

    //    def write(elem: Element[S], out: DataOutput) {
    //      elem.write(out)
    //    }
  }

  private def mkName[S <: Sys[S]](name: _String)(implicit tx: S#Tx): Name[S] =
    Strings.newVar[S](Strings.newConst(name))

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

    final protected def writeData(out: DataOutput) {
      out.writeInt(typeID)
      name.write(out)
      entity.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
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
        pull(name.changed).map(ch => Update(self, IIdxSeq(Renamed(ch))))
      }

      def connect()(implicit tx: S#Tx) {
        name.changed ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        name.changed -/-> this
      }
    }
  }

  private sealed trait PassiveImpl[S <: Sys[S]]
    extends Impl[S] {

    def changed: EventLike[S, Element.Update[S], Element[S]] = NameChange
  }

  private sealed trait ActiveImpl[S <: Sys[S]]
    extends Impl[S] with evt.impl.Reducer[S, Update[S], Update[S], Element[S]] {
    self =>

    // ---- events ----

    protected def entityEvent: evt.EventLike[S, Any, _]

    final protected def events = IIdxSeq(NameChange, EntityChange)
    final protected def changedSlot = 2

    private object EntityChange extends EventImpl {
      final val slot = 1
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        pull(entityEvent).map(ch => Update(self, IIdxSeq(Entity(ch))))
      }

      def connect()(implicit tx: S#Tx) {
        entityEvent ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        entityEvent -/-> this
      }
    }
  }
}
/** Elements are what a document is made from. They comprise heterogenous objects from expressions (integer, double,
  * string etc. expressions), processes and groups of processes, as well as `Element.Group` which is a container
  * for nested elements.
 */
sealed trait Element[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type A

  /** An element always has a variable name attached to it. */
  def name: Expr.Var[S, String]
  /** The actual object wrapped by the element. */
  def entity: A

  /** An event for tracking element changes, which can be renaming
    * the element or forwarding changes from the underlying entity.
    */
  def changed: EventLike[S, Element.Update[S], Element[S]]
}