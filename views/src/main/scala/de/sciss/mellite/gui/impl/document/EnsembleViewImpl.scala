/*
 *  EnsembleViewImpl.scala
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
package gui
package impl
package document

import de.sciss.desktop.UndoManager
import de.sciss.lucre.stm
import de.sciss.lucre.swing.deferTx
import de.sciss.lucre.swing.impl.ComponentHolder
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.Ensemble

import scala.swing.Component

object EnsembleViewImpl {
  def apply[S <: Sys[S]](ensemble: Ensemble[S])(implicit tx: S#Tx, workspace: Workspace[S],
                                                cursor: stm.Cursor[S], undoManager: UndoManager): EnsembleView[S] = {
    val res = new Impl(tx.newHandle(ensemble))
    deferTx {
      res.guiInit()
    }
    res
  }

  private final class Impl[S <: Sys[S]](ensembleH: stm.Source[S#Tx, Ensemble[S]])
                                       (implicit val undoManager: UndoManager, val workspace: Workspace[S],
                                        val cursor: stm.Cursor[S])
    extends ComponentHolder[Component] with EnsembleView[S] {

    def ensemble(implicit tx: S#Tx): Ensemble[S] = ensembleH()

    def guiInit(): Unit = {
      ???
    }

    def dispose()(implicit tx: S#Tx): Unit = ()
  }
}
