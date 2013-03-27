package de.sciss.mellite
package gui

import de.sciss.lucre.stm.{Disposable, Cursor}
import swing.Frame
import de.sciss.synth.proc.{Sys, Proc}
import impl.{ProcEditorFrameImpl => Impl}

object ProcEditorFrame {
  def apply[S <: Sys[S]](proc: Proc[S])(implicit tx: S#Tx, cursor: Cursor[S]): ProcEditorFrame[S] =
    Impl(proc)
}

trait ProcEditorFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: Frame

  def proc(implicit tx: S#Tx): Proc[S]
}
