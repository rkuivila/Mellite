/*
 *  ExprView.scala
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

import de.sciss.lucre.event.{Sys, Observable}
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.model.Change
import de.sciss.serial.Serializer
import de.sciss.synth.proc.{Elem, Obj}
import de.sciss.synth.proc.impl.ObservableImpl

import scala.concurrent.stm.Ref
import scala.reflect.ClassTag
import scala.language.higherKinds

// XXX TODO - should go somewhere else - LucreExpr or LucreSwing
object ExprView {
  def expr[S <: Sys[S], A](x: Expr[S, A])(implicit tx: S#Tx,
                                          serializer: Serializer[S#Tx, S#Acc, Expr[S, A]]): ExprView[S#Tx, A] =
    new ExImpl[S, A, Expr[S, A]](tx.newHandle(x))

  def exprLike[S <: Sys[S], A, Ex <: Expr[S, A]](x: Ex)
                                            (implicit tx: S#Tx,
                                             serializer: Serializer[S#Tx, S#Acc, Ex]): ExprView[S#Tx, A] =
    new ExImpl[S, A, Ex](tx.newHandle(x))

  def attrExpr[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }](obj: Obj[S], key: String)
                              (implicit tx: S#Tx, companion: Elem.Companion[E]): ExprView[S#Tx, Option[A]] =
    new AttrImpl[S, A, E](tx.newHandle(obj), key)

  def const[S <: Sys[S], A](value: A): ExprView[S#Tx, A] = new ConstImpl(value)

  def apply[S <: Sys[S], A](init: A)(implicit tx: S#Tx): Var[S, A] = new VarImpl(init)

  private trait Impl[S <: Sys[S], A] extends ExprView[S#Tx, A] {
    def map[B](f: A => B): ExprView[S#Tx, B] = new MapImpl(this, f)
  }

  private final class ExImpl[S <: Sys[S], A, Ex <: Expr[S, A]](h: stm.Source[S#Tx, Ex])
    extends Impl[S, A] {

    def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      h().changed.react { implicit tx => ch => fun(tx)(ch.now) }

    def apply()(implicit tx: S#Tx): A = h().value
  }

  private final class AttrImpl[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }](
      h: stm.Source[S#Tx, Obj[S]], key: String)(implicit companion: Elem.Companion[E])
    extends Impl[S, Option[A]] {

    private def map[B](aObj: Obj[S])(fun: Expr[S, A] => B): Option[B] = {
      if (aObj.elem.typeID == companion.typeID) Some(fun(aObj.elem.asInstanceOf[E[S]].peer)) else None
      // tag.unapply(aObj.elem.peer).map(fun)
    }

    def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      h().changed.react { implicit tx => ch =>
        ch.changes.foreach {
          case Obj.AttrAdded  (`key`, aObj) => map(aObj) { expr =>
            fun(tx)(Some(expr.value))
          }
          case Obj.AttrRemoved(`key`, aObj) => map(aObj) { expr =>
            fun(tx)(None)
          }
          case Obj.AttrChange (`key`, aObj, changes) =>
            map(aObj) { expr =>
              val hasValue = changes.exists {
                case Obj.ElemChange(Change(_, _)) => true
                case _ => false
              }
              if (hasValue) fun(tx)(Some(expr.value))
            }

          case _ =>
        }
      }

    def apply()(implicit tx: S#Tx): Option[A] = h().attr[E](key).map(_.value)
  }

  private final class MapImpl[S <: Sys[S], A, B](in: ExprView[S#Tx, A], f: A => B)
    extends Impl[S, B] {

    def react(fun: S#Tx => B => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      in.react { implicit tx => a => fun(tx)(f(a)) }

    def apply()(implicit tx: S#Tx): B = f(in())
  }

  object DummyDisposable extends Disposable[Any] {
    def dispose()(implicit tx: Any): Unit = ()
  }

  private final class ConstImpl[S <: Sys[S], A](value: A)
    extends Impl[S, A] {

    def react(fun: (S#Tx) => (A) => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = DummyDisposable

    def apply()(implicit tx: S#Tx): A = value
  }

  trait Var[S <: Sys[S], A] extends ExprView[S#Tx, A] with stm.Sink[S#Tx, A]

  private final class VarImpl[S <: Sys[S], A](init: A)
    extends Var[S, A] with Impl[S, A] with ObservableImpl[S, A] {

    private val ref = Ref(init)

    def apply()(implicit tx: S#Tx): A = ref.get(tx.peer)

    def update(v: A)(implicit tx: S#Tx): Unit = {
      val old = ref.swap(v)(tx.peer)
      if (v != old) fire(v)
    }
  }
}
trait ExprView[Tx, +A] extends Observable[Tx, A] with stm.Source[Tx, A] {
  def map[B](f: A => B): ExprView[Tx, B]
}