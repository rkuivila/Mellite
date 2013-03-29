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

import de.sciss.lucre.{stm, io, event => evt}
import de.sciss.synth.proc.{InMemory, Sys, ProcGroup => _ProcGroup}
import de.sciss.lucre.expr.Expr
import io.{DataOutput, Writable, DataInput}
import stm.{Disposable, Mutable}
import de.sciss.synth.expr.{Doubles, Strings, Ints}
import annotation.switch
import de.sciss.mellite
import evt.{EventLike, EventLikeSerializer}
import collection.immutable.{IndexedSeq => IIdxSeq}


/*
 * Elements
 * - Proc
 * - ProcGroup
 * - primitive expressions
 * -
 */

object Element {
  import scala.{Int => _Int, Double => _Double}
  import java.lang.{String => _String}
  import mellite.{Elements => _Group}

  private final val groupTypeID     = 0x10000
  private final val procGroupTypeID = 0x10001

  type Name[S <: Sys[S]] = Expr.Var[S, _String]

  sealed trait Change[S <: Sys[S]]
  final case class Update[S <: Sys[S]](element: Element[S], changes: IIdxSeq[Change[S]])
  final case class Renamed[S <: Sys[S]](change: evt.Change[_String]) extends Change[S]
  final case class Entity[S <: Sys[S]](change: Any) extends Change[S]

  object Int {
    private[Element] def readIdentified[S <: Sys[S]](in: DataInput, access: S#Acc, targets: evt.Targets[S])
                                                    (implicit tx: S#Tx): Impl[S] = {
      val name    = Strings.readVar[S](in, access)
      val entity = Ints.readVar[S](in, access)
      new Int.Impl(targets, name, entity)
    }

    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Int])(implicit tx: S#Tx): Int[S] = {
      new Impl(evt.Targets[S], mkName(name), Ints.newVar(init))
    }

    // scalac fails with sucky type inference errors when using `int: Int[S]`, so have a fantastic extra match :-E
//    def unapply[S <: Sys[S]](int: Int[S]): Option[Expr.Var[S, _Int]] = Some(int.elem)

    def unapply[S <: Sys[S]](elem: Element.Int[S]): Option[Expr.Var[S, _Int]] = Some(elem.entity)

    implicit def serializer[S <: Sys[S]]: io.Serializer[S#Tx, S#Acc, Int[S]] = anySer.asInstanceOf[Serializer[S]]

    private val anySer = new Serializer[InMemory]
    private final class Serializer[S <: Sys[S]] extends io.Serializer[S#Tx, S#Acc, Int[S]] {
      def write(v: Int[S], out: DataOutput) {
        v.write(out)
      }

      def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Int[S] = {
        val targets = evt.Targets.read[S](in, access)
        val cookie  = in.readByte()
        require(cookie == Ints.typeID, s"Cookie $cookie does not match expected value ${Ints.typeID}")
        readIdentified(in, access, targets)
      }
    }

    private[Element] final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr.Var[S, _Int])
      extends ExprImpl[S, _Int] with Int[S] {
      def prefix = "Int"
      def typeID = Ints.typeID
    }
  }
  sealed trait Int[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _Int] }

  object Double {
    def apply[S <: Sys[S]](name: _String, init: Expr[S, _Double])(implicit tx: S#Tx): Double[S] = {
      new Impl(evt.Targets[S], mkName(name), Doubles.newVar(init))
    }

    def unapply[S <: Sys[S]](elem: Element.Double[S]): Option[Expr.Var[S, _Double]] = Some(elem.entity)

    private[Element] final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr.Var[S, _Double])
      extends ExprImpl[S, _Double] with Double[S] {
      def typeID = Doubles.typeID
      def prefix = "Double"
    }
  }
  sealed trait Double[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _Double] }

  object String {
    def apply[S <: Sys[S]](name: _String, init: Expr[S, _String])(implicit tx: S#Tx): String[S] = {
      new Impl(evt.Targets[S], mkName(name), Strings.newVar(init))
    }

    def unapply[S <: Sys[S]](elem: Element.String[S]): Option[Expr.Var[S, _String]] = Some(elem.entity)

    private[Element] final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: Expr.Var[S, _String])
      extends ExprImpl[S, _String] with String[S] {
      def typeID = Strings.typeID
      def prefix = "String"
    }
  }
  sealed trait String[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _String] }

  object Group {
    def apply[S <: Sys[S]](name: _String, init: _Group[S])(implicit tx: S#Tx): Group[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    def unapply[S <: Sys[S]](elem: Element.Group[S]): Option[_Group[S]] = Some(elem.entity)

    private[Element] final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S], val entity: _Group[S])
      extends Element.Impl[S] with Group[S] {
      self =>

      def typeID = groupTypeID // _Group.typeID
      def prefix = "Group"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait Group[S <: Sys[S]] extends Element[S] { type A = _Group[S] }

  object ProcGroup {
    def apply[S <: Sys[S]](name: _String, init: _ProcGroup[S])(implicit tx: S#Tx): ProcGroup[S] = {
      new Impl(evt.Targets[S], mkName(name), init)
    }

    def unapply[S <: Sys[S]](elem: Element.ProcGroup[S]): Option[_ProcGroup[S]] = Some(elem.entity)

    private[Element] final class Impl[S <: Sys[S]](val targets: evt.Targets[S], val name: Name[S],
                                                   val entity: _ProcGroup[S])
      extends Element.Impl[S] with ProcGroup[S] {
      self =>

      def typeID = procGroupTypeID
      def prefix = "ProcGroup"

      protected def entityEvent = entity.changed
    }
  }
  sealed trait ProcGroup[S <: Sys[S]] extends Element[S] { type A = _ProcGroup[S] }


//  def group[S <: Sys[S]](init: Group[S], name: Option[String] = None)
//                        (implicit tx: S#Tx): Element[S] {type A = Group[S]} = {
//    mkImpl[S, Group[S]](Group.typeID, name, init)
//  }

//  implicit def serializer[S <: Sys[S]]: stm.Serializer[S#Tx, S#Acc, Element[S]] = anySer.asInstanceOf[Ser[S]]
  implicit def serializer[S <: Sys[S]]: evt.Serializer[S, Element[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends EventLikeSerializer[S, Element[S]] {
    def readConstant(in: DataInput)(implicit tx: S#Tx): Element[S] = {
      sys.error("No passive elements known")
    }

    def read(in: DataInput, access: S#Acc, targets: evt.Targets[S])(implicit tx: S#Tx): Element[S] with evt.Node[S] = {
      val typeID  = in.readInt()
      (typeID: @switch) match {
        case Ints.typeID => Int.readIdentified[S](in, access, targets)
        case Doubles.typeID =>
          val name    = Strings.readVar[S](in, access)
          val entity = Doubles.readVar[S](in, access)
          new Double.Impl(targets, name, entity)
        case Strings.typeID =>
          val name    = Strings.readVar[S](in, access)
          val entity = Strings.readVar[S](in, access)
          new String.Impl(targets, name, entity)
        case `groupTypeID` /* _Group.typeID */ =>
          val name    = Strings.readVar[S](in, access)
          val entity = _Group.read[S](in, access)
          new Group.Impl(targets, name, entity)
        case `procGroupTypeID` =>
          val name    = Strings.readVar[S](in, access)
          val entity = _ProcGroup.read[S](in, access)
          new ProcGroup.Impl(targets, name, entity)
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
//    new Impl(id, nameEx, typeID, elem)
//  }

  private sealed trait ExprImpl[S <: Sys[S], A1] extends Impl[S] {
    self =>
    type A <: Expr[S, A1]
    final protected def entityEvent = entity.changed
  }

  private sealed trait Impl[S <: Sys[S]]
    extends Element[S] with evt.Node[S] with evt.impl.MultiEventImpl[S, Update[S], Update[S], Element[S]] {
    self =>

    type A <: Writable with Disposable[S#Tx]

//    /* final */ type A = A1

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

//    final def changed: EventLike[S, Element.Update[S], Element[S]] = this

    // ---- events ----

    protected def entityEvent: evt.EventLike[S, Any, _]

    final protected def events = IIdxSeq(NameChange, EntityChange)

    private object EntityChange extends EventImpl {
      final val slot = 2
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        entityEvent.pullUpdate(pull).map(ch => Update(self, IIdxSeq(Entity(ch))))
      }

      def connect()(implicit tx: S#Tx) {
        entityEvent ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        entityEvent -/-> this
      }
    }

    final protected def reader: evt.Reader[S, Element[S]] = serializer

    final protected def foldUpdate(sum: Option[Update[S]], inc: Update[S]): Option[Update[S]] = sum match {
      case Some(prev) => Some(prev.copy(changes = prev.changes ++ inc.changes))
      case _          => Some(inc)
    }

    trait EventImpl
      extends evt.impl.EventImpl[S, Update[S], Element[S]] with evt.InvariantEvent[S, Update[S], Element[S]] {

      final protected def reader: evt.Reader[S, Element[S]] = self.reader
      final def node: Element[S] with evt.Node[S] = self
    }

    protected object NameChange extends EventImpl {
      final val slot = 1
      def pullUpdate(pull: evt.Pull[S])(implicit tx: S#Tx): Option[Update[S]] = {
        name.changed.pullUpdate(pull).map(ch => Update(self, IIdxSeq(Renamed(ch))))
      }

      def connect()(implicit tx: S#Tx) {
        name.changed ---> this
      }

      def disconnect()(implicit tx: S#Tx) {
        name.changed -/-> this
      }
    }
  }
}

sealed trait Element[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type A

  def name: Expr.Var[S, String]
  def entity: A
  def changed: EventLike[S, Element.Update[S], Element[S]]
}