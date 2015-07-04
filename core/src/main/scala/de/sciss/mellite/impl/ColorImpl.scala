/*
 *  ColorImpl.scala
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
package impl

import de.sciss.lucre.event.Sys
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.{event => evt}
import de.sciss.synth.proc.Elem
import de.sciss.synth.proc.impl.{ActiveElemImpl, ExprElemCompanionImpl, PassiveElemImpl}

object ColorImpl extends ExprElemCompanionImpl[Color.Elem, Color] {
  protected val tpe = Color.Expr

  private lazy val _init: Unit = Elem.registerExtension(this)
  def init(): Unit = _init

  protected def newActive[S <: Sys[S]](targets: evt.Targets[S], peer: Expr[S, Color])
                                      (implicit tx: S#Tx): Color.Elem[S] with evt.Node[S] =
    new ActiveImpl(targets, peer)

  protected def newConst[S <: Sys[S]](peer: Expr.Const[S, Color])(implicit tx: S#Tx): Color.Elem[S] =
    new ConstImpl[S](peer)

  private trait Impl[S <: Sys[S]] extends Color.Elem[S] {
    final def prefix = "Color"
    final def typeID = Color.typeID
  }

  private final class ConstImpl[S <: Sys[S]](val peer: Expr.Const[S, Color])
    extends Impl[S] with PassiveElemImpl[S, Color.Elem[S]]

  private final class ActiveImpl[S <: Sys[S]](val targets: evt.Targets[S], val peer: Expr[S, Color])
    extends ActiveElemImpl[S] with Impl[S] {

    def mkCopy()(implicit tx: S#Tx): Color.Elem[S] = Color.Elem(ColorImpl.copyExpr(peer))
  }
}