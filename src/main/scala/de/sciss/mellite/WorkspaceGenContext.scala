/*
 *  WorkspaceGenContext.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2017 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v3+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite

import de.sciss.lucre.stm
import de.sciss.lucre.stm.TxnLike.peer
import de.sciss.lucre.stm.{Disposable, Sys}
import de.sciss.synth.proc.{GenContext, WorkspaceHandle}

import scala.concurrent.stm.TMap

object WorkspaceGenContext {
  private[this] val map = TMap.empty[WorkspaceHandle[_], GenContext[_]]

  implicit def apply[S <: Sys[S]](implicit tx: S#Tx, ws: WorkspaceHandle[S], cursor: stm.Cursor[S]): GenContext[S] = {
    val res = map.get(ws).getOrElse {
      val res0 = GenContext[S]
      ws.addDependent(new Disposable[S#Tx] {
        def dispose()(implicit tx: S#Tx): Unit = map.remove(ws)
      })
      map.put(ws, res0)
      res0
    }
    res.asInstanceOf[GenContext[S]]
  }
}