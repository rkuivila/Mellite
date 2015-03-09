/*
 *  AttrCellViewImpl.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.{ExprType, Expr}
import de.sciss.lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.CellView
import de.sciss.lucre.swing.impl.CellViewImpl
import de.sciss.model.Change
import de.sciss.serial.Serializer
import de.sciss.synth.proc.{Elem, Obj}

import scala.language.higherKinds

object AttrCellViewImpl {
  private[gui] trait Basic[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }]
    extends CellViewImpl.Basic[S#Tx, Option[A]] {

    protected def h: stm.Source[S#Tx, Obj[S]]
    protected val key: String

    implicit protected def companion: Elem.Companion[E]

    type Repr = Option[Expr[S, A]]

    private def map[B](aObj: Obj[S])(fun: Expr[S, A] => B): Option[B] =
      if (aObj.elem.typeID == companion.typeID) Some(fun(aObj.elem.asInstanceOf[E[S]].peer)) else None

    def react(fun: S#Tx => Option[A] => Unit)(implicit tx: S#Tx): Disposable[S#Tx] =
      h().changed.react { implicit tx => u =>
        u.changes.foreach {
          case Obj.AttrAdded  (`key`, aObj) =>
            map(aObj) { ex =>
              fun(tx)(Some(ex.value))
            }
          case Obj.AttrRemoved(`key`, aObj) =>
            map(aObj) { ex =>
              fun(tx)(None)
            }
          case Obj.AttrChange(`key`, aObj, attrChanges) =>
            map(aObj) { ex =>
              val hasValue = attrChanges.exists {
                case Obj.ElemChange(Change(_, _)) => true
                case _ => false
              }
              if (hasValue) fun(tx)(Some(ex.value))
            }
          case _ =>
        }
      }

    def repr(implicit tx: S#Tx): Repr = h().attr[E](key)

    def apply()(implicit tx: S#Tx): Option[A] = repr.map(_.value)
  }

  //  // actually unused, because obj.attr is always modifiable
  //  private[gui] final class Impl[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }](
  //         protected val h: stm.Source[S#Tx, Obj[S]],
  //         protected val key: String)(implicit protected val companion: Elem.Companion[E])
  //    extends Basic[S, A, E]

  private[gui] final class ModImpl[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }](
         protected val h: stm.Source[S#Tx, Obj[S]],
         protected val key: String)(implicit tpe: ExprType[A], protected val companion: Elem.Companion[E])
    extends Basic[S, A, E] with CellView.Var[S, Option[A]] {

    def serializer: Serializer[S#Tx, S#Acc, Repr] = {
      implicit val exSer = tpe.serializer[S]
      Serializer.option[S#Tx, S#Acc, Expr[S, A]]
    }

    protected def mapUpdate(ch: Change[A]): Option[A] = if (ch.isSignificant) Some(ch.now) else None

    def repr_=(value: Repr)(implicit tx: S#Tx): Unit = value.fold[Unit] {
      h().attr.remove(key)
    } { ex =>
      val map = h().attr
      map.apply[E](key) match {
        case Some(Expr.Var(vr)) => vr() = ex
        case _ =>
          val exV   = Expr.Var.unapply[S, A](ex).getOrElse(tpe.newVar(ex)): E[S]#Peer
          // XXX .asInstanceOf -- WTF?
          // cf. https://stackoverflow.com/questions/28945416/type-mismatch-with-type-projection?stw=2
          val elem  = companion.asInstanceOf[Elem.Companion[Elem]].apply[S](exV)
          val aObj  = Obj(elem)
          map.put(key, aObj)
      }
    }

    def lift(value: Option[A])(implicit tx: S#Tx): Repr = value.map(tpe.newConst)

    def update(v: Option[A])(implicit tx: S#Tx): Unit = repr_=(lift(v))
  }
}