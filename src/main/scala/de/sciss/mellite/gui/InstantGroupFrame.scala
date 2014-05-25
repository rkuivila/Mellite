/*
 *  InstantGroupFrame.scala
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

import impl.realtime.{FrameImpl => Impl}
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm
import de.sciss.desktop.Window
import de.sciss.lucre.swing.View
import de.sciss.synth.proc.{ProcGroupElem, Obj}

object InstantGroupFrame {
  def apply[S <: Sys[S]](document: Document[S], group: Obj.T[S, ProcGroupElem] /*, transport: Document.Transport[S] */)
                        (implicit tx: S#Tx, cursor: stm.Cursor[S]): InstantGroupFrame[S] =
    Impl(document, group)
}

trait InstantGroupFrame[S <: Sys[S]] extends View[S] {
  def window: Window
  // def group(implicit tx: S#Tx): Document.Group[S]
  // def transport /* ( implicit tx: S#Tx ) */ : Document.Transport[S]
  def contents: InstantGroupPanel[S]
}
