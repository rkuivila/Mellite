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
import de.sciss.lucre.stm.{TxnLike, Disposable}
import de.sciss.model.Change
import de.sciss.serial.Serializer
import de.sciss.synth.proc.{ObjKeys, StringElem, Elem, Obj}
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

  def name[S <: Sys[S]](obj: Obj[S], prefix: String = "", suffix: String = "")
                       (implicit tx: S#Tx): ExprView[S#Tx, String] = {
    val ex1 = attrExpr[S, String, StringElem](obj, ObjKeys.attrName).map(_.getOrElse("<unnamed>"))
    if (prefix.isEmpty && suffix.isEmpty) ex1 else ex1.map { name => s"$prefix$name$suffix" }
  }

  def const[S <: Sys[S], A](value: A): ExprView[S#Tx, A] = new ConstImpl(value)

  def apply[S <: Sys[S], A](init: A)(implicit tx: S#Tx): Var[S, A] = new VarImpl(init)

  private trait Impl[Tx, A] extends ExprView[Tx, A] {
    def map[B](f: A => B): ExprView[Tx, B] = new MapImpl(this, f)
    // def mapTx[Tx1](f: Tx1 => Tx): ExprView[Tx1, A] = new MapTxImpl[Tx1, Tx, A](this, f)
  }

  private final class ExImpl[S <: Sys[S], A, Ex <: Expr[S, A]](h: stm.Source[S#Tx, Ex])
    extends Impl[S#Tx, A]  {

    def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      h().changed.react { implicit tx => ch => fun(tx)(ch.now) }

    def apply()(implicit tx: S#Tx): A = h().value
  }

  private final class AttrImpl[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }](
      h: stm.Source[S#Tx, Obj[S]], key: String)(implicit companion: Elem.Companion[E])
    extends Impl[S#Tx, Option[A]] {

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

  private final class MapImpl[Tx, A, B](in: ExprView[Tx, A], f: A => B)
    extends Impl[Tx, B] {

    def react(fun: Tx => B => Unit)(implicit tx: Tx): Disposable[Tx] =
      in.react { implicit tx => a => fun(tx)(f(a)) }

    def apply()(implicit tx: Tx): B = f(in())
  }

  //  private final class MapTxImpl[Tx, TxIn, A](in: ExprView[TxIn, A], f: Tx => TxIn)
  //    extends Impl[Tx, A] {
  //
  //    def react(fun: Tx => A => Unit)(implicit tx: Tx): Disposable[Tx] =
  //      in.react { implicit tx => a => fun(tx) }
  //
  //    def apply()(implicit tx: Tx): A = in()(f(tx))
  //  }

  object DummyDisposable extends Disposable[Any] {
    def dispose()(implicit tx: Any): Unit = ()
  }

  private final class ConstImpl[S <: Sys[S], A](value: A)
    extends Impl[S#Tx, A] with ExprView[S#Tx, A] {

    def react(fun: S#Tx => A => Unit)(implicit tx: S#Tx): Disposable[S#Tx] = DummyDisposable

    def apply()(implicit tx: S#Tx): A = value
  }

  trait Var[S <: Sys[S], A] extends ExprView[S#Tx, A] with stm.Sink[S#Tx, A]

  private final class VarImpl[S <: Sys[S], A](init: A)
    extends Var[S, A] with Impl[S#Tx, A] with ObservableImpl[S, A] {

    private val ref = Ref(init)

    def apply()(implicit tx: S#Tx): A = ref.get(tx.peer)

    def update(v: A)(implicit tx: S#Tx): Unit = {
      val old = ref.swap(v)(tx.peer)
      if (v != old) fire(v)
    }
  }

  //  trait Like[Tx <: TxnLike, A] extends Observable[TxnLike, A] with stm.Source[Tx, A] {
  //    def map[B](f: A => B): Like[Tx, B]
  //    def mapTx[Tx1](f: Tx1 => Tx): Like[Tx1, A]
  //  }
}
trait ExprView[Tx, +A] extends Observable[Tx, A] with stm.Source[Tx, A] {
  def map[B](f: A => B): ExprView[Tx, B]
}