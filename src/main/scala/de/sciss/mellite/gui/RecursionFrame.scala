/*
 *  RecursionFrame.scala
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

import de.sciss.synth.proc.{Obj, AuralSystem}
import lucre.stm
import stm.Disposable
import impl.{RecursionFrameImpl => Impl}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.swing.View

object RecursionFrame {
  def apply[S <: Sys[S]](doc: Document[S], obj: Obj.T[S, Recursion.Elem])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): RecursionFrame[S] =
    Impl(doc, obj)
}

trait RecursionFrame[S <: Sys[S]] extends View[S] {
  def window   : desktop.Window
  def document : Document[S]
}
