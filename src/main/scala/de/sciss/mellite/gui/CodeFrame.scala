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
import stm.Disposable
import impl.interpreter.{CodeFrameImpl => Impl}
import de.sciss.lucre.synth.Sys

object CodeFrame {
  def apply[S <: Sys[S]](doc: Document[S], elem: Element.Code[S])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): CodeFrame[S] =
    Impl(doc, elem)
}

trait CodeFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
