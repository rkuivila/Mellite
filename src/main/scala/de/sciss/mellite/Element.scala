package de.sciss.mellite

import de.sciss.lucre.{DataInput, stm, DataOutput, Writable}
import de.sciss.synth.proc.{InMemory, Sys}
import de.sciss.lucre.expr.Expr
import stm.{Disposable, Mutable}
import de.sciss.synth.expr.{Doubles, Strings, Ints}
import annotation.switch
import de.sciss.mellite

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

  private final val groupTypeID = 0x10000

  type Name[S <: Sys[S]] = Expr.Var[S, Option[_String]]

  object Int {
    def apply[S <: Sys[S]](init: Expr[S, _Int], name: Option[_String] = None)(implicit tx: S#Tx): Int[S] = {
      new Impl(tx.newID(), mkName(name), Ints.newVar(init))
    }

    // scalac fails with sucky type inference errors when using `int: Int[S]`, so have a fantastic extra match :-E
//    def unapply[S <: Sys[S]](int: Int[S]): Option[Expr.Var[S, _Int]] = Some(int.elem)

    def unapply[S <: Sys[S]](elem: Element[S]): Option[Expr.Var[S, _Int]] = elem match {
      case i: Int[S] => Some(i.entity)
      case _ => None
    }

    private[Element] final class Impl[S <: Sys[S]](val id: S#ID, val name: Name[S], val entity: Expr.Var[S, _Int])
      extends Element.Impl[S] with Int[S] {
      def typeID = Ints.typeID
      override def toString() = "Element.Int" + id
    }
  }
  sealed trait Int[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _Int] }

  object Double {
    def apply[S <: Sys[S]](init: Expr[S, _Double], name: Option[_String] = None)(implicit tx: S#Tx): Double[S] = {
      new Impl(tx.newID(), mkName(name), Doubles.newVar(init))
    }

    def unapply[S <: Sys[S]](elem: Element[S]): Option[Expr.Var[S, _Double]] = elem match {
      case d: Double[S] => Some(d.entity)
      case _ => None
    }

    private[Element] final class Impl[S <: Sys[S]](val id: S#ID, val name: Name[S], val entity: Expr.Var[S, _Double])
      extends Element.Impl[S] with Double[S] {
      def typeID = Doubles.typeID
      override def toString() = "Element.Double" + id
    }
  }
  sealed trait Double[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _Double] }

  object String {
    def apply[S <: Sys[S]](init: Expr[S, _String], name: Option[_String] = None)(implicit tx: S#Tx): String[S] = {
      new Impl(tx.newID(), mkName(name), Strings.newVar(init))
    }

    def unapply[S <: Sys[S]](elem: Element[S]): Option[Expr.Var[S, _String]] = elem match {
      case s: String[S] => Some(s.entity)
      case _ => None
    }

    private[Element] final class Impl[S <: Sys[S]](val id: S#ID, val name: Name[S], val entity: Expr.Var[S, _String])
      extends Element.Impl[S] with String[S] {
      def typeID = Strings.typeID
      override def toString() = "Element.String" + id
    }
  }
  sealed trait String[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, _String] }

  object Group {
    def apply[S <: Sys[S]](init: _Group[S], name: Option[_String] = None)(implicit tx: S#Tx): Group[S] = {
      new Impl(tx.newID(), mkName(name), init)
    }

    def unapply[S <: Sys[S]](elem: Element[S]): Option[_Group[S]] = elem match {
      case g: Group[S] => Some(g.entity)
      case _ => None
    }

    private[Element] final class Impl[S <: Sys[S]](val id: S#ID, val name: Name[S], val entity: _Group[S])
      extends Element.Impl[S] with Group[S] {
      def typeID = groupTypeID // _Group.typeID
      override def toString() = "Element.Group" + id
    }
  }
  sealed trait Group[S <: Sys[S]] extends Element[S] { type A = _Group[S] }

//
//  def group[S <: Sys[S]](init: Group[S], name: Option[String] = None)
//                        (implicit tx: S#Tx): Element[S] {type A = Group[S]} = {
//    mkImpl[S, Group[S]](Group.typeID, name, init)
//  }

  implicit def serializer[S <: Sys[S]]: stm.Serializer[S#Tx, S#Acc, Element[S]] = anySer.asInstanceOf[Ser[S]]

  private final val anySer = new Ser[InMemory]

  private final class Ser[S <: Sys[S]] extends stm.Serializer[S#Tx, S#Acc, Element[S]] {
    def read(in: DataInput, access: S#Acc)(implicit tx: S#Tx): Element[S] = {
      val id      = tx.readID(in, access)
      val typeID  = in.readInt()
      val name    = StringOptions.readVar[S](in, access)
      (typeID: @switch) match {
        case Ints.typeID =>
          val entity = Ints.readVar[S](in, access)
          new Int.Impl(id, name, entity)
        case Doubles.typeID =>
          val entity = Doubles.readVar[S](in, access)
          new Double.Impl(id, name, entity)
        case Strings.typeID =>
          val entity = Strings.readVar[S](in, access)
          new String.Impl(id, name, entity)
        case `groupTypeID` /* _Group.typeID */ =>
          val entity = _Group.read[S](in, access)
          new Group.Impl(id, name, entity)
      }
    }

    def write(elem: Element[S], out: DataOutput) {
      elem.write(out)
    }
  }

  private def mkName[S <: Sys[S]](name: Option[_String])(implicit tx: S#Tx): Expr.Var[S, Option[_String]] =
    StringOptions.newVar[S]( StringOptions.newConst(name))

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

  private sealed trait Impl[S <: Sys[S]]
//  (val id: S#ID, val name: Expr.Var[S, Option[String]], val typeID: Int,
//                                             val elem: A1 with Writable with Disposable[S#Tx])
    extends Element[S] with Mutable.Impl[S] {

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
  }
}

sealed trait Element[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type A

  def name: Expr.Var[S, Option[String]]
  def entity: A
}