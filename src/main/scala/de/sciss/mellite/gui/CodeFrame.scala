/*
 *  CodeFrame.scala
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

package de.sciss
package mellite
package gui

import lucre.stm
import impl.interpreter.{CodeFrameImpl => Impl}
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{Proc, Obj}

object CodeFrame {
  def apply[S <: Sys[S]](obj: Obj.T[S, Code.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] =
    Impl(obj)

  def proc[S <: Sys[S]](proc: Obj.T[S, Proc.Elem])
                        (implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): CodeFrame[S] =
    Impl.proc(proc)
}

trait CodeFrame[S <: Sys[S]] extends lucre.swing.Window[S] {
  def view: CodeView[S]
}
