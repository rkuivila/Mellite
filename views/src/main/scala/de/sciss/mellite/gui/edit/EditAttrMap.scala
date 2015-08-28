/*
 *  EditAttrMap.scala
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
package edit

import javax.swing.undo.{AbstractUndoableEdit, UndoableEdit}

import de.sciss.lucre.expr.Expr
import de.sciss.lucre.stm.{Obj, Sys}
import de.sciss.lucre.{event => evt, stm}
import de.sciss.serial.Serializer
import org.scalautils.TypeCheckedTripleEquals

import scala.language.higherKinds

object EditAttrMap {
  def apply[S <: Sys[S]](name: String, obj: Obj[S], key: String, value: Option[Obj[S]])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val before    = obj.attr.get(key)
    val objH      = tx.newHandle(obj)
    val beforeH   = tx.newHandle(before)
    val nowH      = tx.newHandle(value)
    val res       = new ApplyImpl(name, key, objH, beforeH, nowH)
    res.perform()
    res
  }

  def expr[S <: Sys[S], A, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, A] }](name: String, obj: Obj[S],
                                                                                 key: String, value: Option[Expr[S, A]])
                          (mkElem: Expr[S, A] => E[S])
                          (implicit tx: S#Tx, cursor: stm.Cursor[S], companion: Elem.Companion[E],
                           serializer: Serializer[S#Tx, S#Acc, Expr[S, A]]): UndoableEdit = {
    // what we do in `expr` is preserve an existing variable.
    // that is, if there is an existing value which is a variable,
    // we do not overwrite that value, but preserve that
    // variable's current child and overwrite that variable's child.
    val befOpt: Option[Expr[S, A]] = obj.attr.$[E](key)
    val before    = befOpt match {
      case Some(Expr.Var(vr)) => Some(vr())
      case other => other
    }
    val objH      = tx.newHandle(obj)
    val beforeH   = tx.newHandle(before)
    val nowH      = tx.newHandle(value)
    val res       = new ExprImpl[S, A, E](name, key, objH, beforeH, nowH, mkElem)
    res.perform()
    res
  }

  private final class ApplyImpl[S <: Sys[S]](val name: String, val key: String,
                                             val objH   : stm.Source[S#Tx, Obj[S]],
                                             val beforeH: stm.Source[S#Tx, Option[Obj[S]]],
                                             val nowH   : stm.Source[S#Tx, Option[Obj[S]]])
                                            (implicit val cursor: stm.Cursor[S])
    extends Impl[S, Obj[S]] {

    protected def put(map: AttrMap.Modifiable[S], elem: Obj[S])(implicit tx: S#Tx): Unit =
      map.put(key, elem)
  }

  private final class ExprImpl[S <: Sys[S], B, E[~ <: Sys[~]] <: Elem[~] { type Peer = Expr[~, B] }](
                                               val name: String, val key: String,
                                               val objH   : stm.Source[S#Tx, Obj[S]],
                                               val beforeH: stm.Source[S#Tx, Option[Expr[S, B]]],
                                               val nowH   : stm.Source[S#Tx, Option[Expr[S, B]]],
                                               mkElem: Expr[S, B] => Elem[S] { type Peer = Expr[S, B]})
                                              (implicit val cursor: stm.Cursor[S], companion: Elem.Companion[E])
    extends Impl[S, Expr[S, B]] {

    protected def put(map: AttrMap.Modifiable[S], elem: Expr[S, B])(implicit tx: S#Tx): Unit = {
      val opt = map[E](key)
      opt match {
        case Some(Expr.Var(vr)) =>
          // see above for an explanation about how we preserve a variable
          import TypeCheckedTripleEquals._
          if (vr === elem) throw new IllegalArgumentException(s"Cyclic reference setting variable $vr")
          vr() = elem
        case _ => map.put(key, Obj(mkElem(elem)))
      }
    }
  }

  private abstract class Impl[S <: Sys[S], A] extends AbstractUndoableEdit {
    protected def name   : String
    protected def key    : String
    protected def objH   : stm.Source[S#Tx, Obj[S]]
    protected def beforeH: stm.Source[S#Tx, Option[A]]
    protected def nowH   : stm.Source[S#Tx, Option[A]]

    protected def cursor: stm.Cursor[S]

    override def undo(): Unit = {
      super.undo()
      cursor.step { implicit tx => perform(beforeH) }
    }

    override def redo(): Unit = {
      super.redo()
      cursor.step { implicit tx => perform() }
    }

    protected def put(map: AttrMap.Modifiable[S], elem: A)(implicit tx: S#Tx): Unit

    private def perform(valueH: stm.Source[S#Tx, Option[A]])(implicit tx: S#Tx): Unit = {
      val map = objH().attr
      valueH().fold[Unit] {
        map.remove(key)
      } { obj =>
        put(map, obj)
      }
    }

    def perform()(implicit tx: S#Tx): Unit = perform(nowH)

    override def getPresentationName = name
  }
}
