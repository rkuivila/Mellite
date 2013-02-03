package de.sciss.mellite

import de.sciss.lucre.{DataInput, stm, DataOutput, Writable}
import de.sciss.synth.proc.{InMemory, Sys}
import de.sciss.lucre.expr.Expr
import stm.{Disposable, Mutable}
import de.sciss.synth.expr.{Doubles, Strings, Ints}
import annotation.switch
import de.sciss.lucre.bitemp.BiType

/*
 * Elements
 * - Proc
 * - ProcGroup
 * - primitive expressions
 * -
 */

object Element {
  import scala.{Int => SInt, Double => SDouble}
  import java.lang.{String => SString}

  type Name[S <: Sys[S]] = Expr.Var[S, Option[SString]]

  object Int {
    def apply[S <: Sys[S]](init: Expr[S, SInt], name: Option[SString] = None)(implicit tx: S#Tx): Int[S] = {
      new Impl(tx.newID(), mkName(name), Ints.newVar(init))
    }

    // scalac fails with sucky type inference errors when using `int: Int[S]`, so have a fantastic extra match :-E
//    def unapply[S <: Sys[S]](int: Int[S]): Option[Expr.Var[S, SInt]] = Some(int.elem)

    def unapply[S <: Sys[S]](elem: Element[S]): Option[Expr.Var[S, SInt]] = elem match {
      case int: Int[S] => Some(int.elem)
      case _ => None
    }

    private final class Impl[S <: Sys[S]](val id: S#ID, val name: Name[S], val elem: Expr.Var[S, SInt])
      extends Element.Impl[S] with Int[S] {
      def typeID = Ints.typeID
    }
  }
  sealed trait Int[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, SInt] }

//  object Double {
//    def double[S <: Sys[S]](init: Expr[S, Double], name: Option[String] = None)
//                           (implicit tx: S#Tx): Element[S] {type A = Expr.Var[S, Double]} = {
//      mkExpr[S, Double](Doubles, init, name)
//    }
//  }
//  sealed trait Double[S <: Sys[S]] extends Element[S] { type A = Expr.Var[S, SDouble] }
//
//  def string[S <: Sys[S]](init: Expr[S, String], name: Option[String] = None)
//                         (implicit tx: S#Tx): Element[S] {type A = Expr.Var[S, String]} = {
//    mkExpr(Strings, init, name)
//  }
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
      val nameEx  = StringOptions.readVar[S](in, access)
      val elem    = (typeID: @switch) match {
        case Ints.typeID    => Ints.readVar[   S](in, access)
        case Doubles.typeID => Doubles.readVar[S](in, access)
        case Strings.typeID => Strings.readVar[S](in, access)
        case Group.typeID   => Group.read[     S](in, access)
      }
//      new Impl(id, nameEx, typeID, elem)
      ???
    }

    def write(elem: Element[S], out: DataOutput) {
      elem.write(out)
    }
  }

  private def mkName[S <: Sys[S]](name: Option[String])(implicit tx: S#Tx): Expr.Var[S, Option[String]] =
    StringOptions.newVar[S]( StringOptions.newConst(name))

//  // A[ ~ <: Sys[ ~ ] forSome { type ~ }]
//  private def mkExpr[S <: Sys[S], A1](biType: BiType[A1], init: Expr[S, A1], name: Option[String])
//                                     (implicit tx: S#Tx) : (SInt, Expr.Var[S, Option[String]], Expr.Var[S, A1]) = {
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

    protected def typeID: SInt

    final protected def writeData(out: DataOutput) {
      out.writeInt(typeID)
      name.write(out)
      elem.write(out)
    }

    final protected def disposeData()(implicit tx: S#Tx) {
      name.dispose()
      elem.dispose()
    }
  }
}

sealed trait Element[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type A

  def name: Expr.Var[S, Option[String]]
  def elem: A
}