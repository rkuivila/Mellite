/*
 *  AuralActionImpl.scala
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
package impl

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt}
import de.sciss.lucre.stm
import de.sciss.synth.proc.impl.ObservableImpl
import de.sciss.synth.proc.{TimeRef, AuralContext, AuralObj}
import de.sciss.synth.proc.Implicits._

import scala.concurrent.stm.Ref

object AuralActionImpl extends AuralObj.Factory {
  AuralObj.addFactory(this)

  type E[S <: evt.Sys[S]] = Action.Elem[S]
  def typeID = Action.typeID

  def apply[S <: Sys[S]](obj: Action.Obj[S])(implicit tx: S#Tx, context: AuralContext[S]): AuralObj[S] = {
    val objH = tx.newHandle(obj)
    new Impl(objH)
  }

  private final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Action.Obj[S]])
    extends AuralObj[S] with ObservableImpl[S, AuralObj.State] {

    def typeID = Action.typeID

    private val stateRef = Ref[AuralObj.State](AuralObj.Stopped)

    def play(timeRef: TimeRef)(implicit tx: S#Tx): Unit = {
      val oldState = stateRef.swap(AuralObj.Playing)(tx.peer)
      if (oldState != AuralObj.Playing) {
        val actionObj = obj()
        if (!actionObj.attr.muted) {
          val action = actionObj.elem.peer
          action.execute()
        }
      }
    }

    def stop()(implicit tx: S#Tx): Unit =
      stateRef.set(AuralObj.Stopped)(tx.peer)

    def state(implicit tx: S#Tx): AuralObj.State = stateRef.get(tx.peer)

    def dispose()(implicit tx: S#Tx) = ()
  }
}