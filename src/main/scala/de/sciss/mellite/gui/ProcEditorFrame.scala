/*
 *  ProcEditorFrame.scala
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

import de.sciss.lucre.stm.{Disposable, Cursor}
import swing.Frame
import de.sciss.synth.proc.Proc
import impl.{ProcEditorFrameImpl => Impl}
import de.sciss.lucre.synth.Sys

object ProcEditorFrame {
  def apply[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx, cursor: Cursor[S]): ProcEditorFrame[S] =
    Impl(proc)
}

trait ProcEditorFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Frame

  def proc(implicit tx: S#Tx): Proc[S]
}
