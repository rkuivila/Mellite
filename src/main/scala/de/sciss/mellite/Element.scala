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
  def int[S <: Sys[S]](init: Expr[S, Int], name: Option[String] = None)
                      (implicit tx: S#Tx): Element[S] {type A = Expr.Var[S, Int]} = {
    mkExpr[S, Int](Ints, init, name)
  }

  def double[S <: Sys[S]](init: Expr[S, Double], name: Option[String] = None)
                         (implicit tx: S#Tx): Element[S] {type A = Expr.Var[S, Double]} = {
    mkExpr[S, Double](Doubles, init, name)
  }

  def string[S <: Sys[S]](init: Expr[S, String], name: Option[String] = None)
                         (implicit tx: S#Tx): Element[S] {type A = Expr.Var[S, String]} = {
    mkExpr(Strings, init, name)
  }

  def group[S <: Sys[S]](init: Group[S], name: Option[String] = None)
                        (implicit tx: S#Tx): Element[S] {type A = Group[S]} = {
    mkImpl[S, Group[S]](Group.typeID, name, init)
  }

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
      new Impl(id, nameEx, typeID, elem)
    }

    def write(elem: Element[S], out: DataOutput) {
      elem.write(out)
    }
  }

  private def mkName[S <: Sys[S]](name: Option[String])(implicit tx: S#Tx): Expr.Var[S, Option[String]] =
    StringOptions.newVar[S]( StringOptions.newConst(name))

  // A[ ~ <: Sys[ ~ ] forSome { type ~ }]
  private def mkExpr[S <: Sys[S], A1](biType: BiType[A1], init: Expr[S, A1], name: Option[String])
                                     (implicit tx: S#Tx) : Element[ S ] { type A = Expr.Var[S, A1]} = {
    val expr = biType.newVar[S](init)
    mkImpl(biType.typeID, name, expr)
}

  private def mkImpl[S <: Sys[S], A1](typeID: Int, name: Option[String],
                                      elem: A1 with Writable with Disposable[S#Tx])
                                     (implicit tx: S#Tx): Element[S] { type A = A1 } = {
    val id      = tx.newID()
    val nameEx  = mkName(name)
    new Impl(id, nameEx, typeID, elem)
  }

  private final class Impl[S <: Sys[S], A1](val id: S#ID, val name: Expr.Var[S, Option[String]], typeID: Int,
                                            val elem: A1 with Writable with Disposable[S#Tx])
    extends Element[S] with Mutable.Impl[S] {

    type A = A1

    protected def writeData(out: DataOutput) {
      out.writeInt(typeID)
      name.write(out)
      elem.write(out)
    }

    protected def disposeData()(implicit tx: S#Tx) {
      name.dispose()
      elem.dispose()
    }
  }
}

trait Element[S <: Sys[S]] extends Mutable[S#ID, S#Tx] {
  type A

  def name: Expr.Var[S, Option[String]]
  def elem: A
}