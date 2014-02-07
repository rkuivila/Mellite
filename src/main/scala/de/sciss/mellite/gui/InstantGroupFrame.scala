/*
 *  InstantGroupFrame.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import swing.Frame
import de.sciss.lucre.stm.Cursor
import impl.realtime.{FrameImpl => Impl}
import de.sciss.lucre.synth.Sys

object InstantGroupFrame {
  def apply[S <: Sys[S]](group: Document.Group[S], transport: Document.Transport[S])
                        (implicit tx: S#Tx, cursor: Cursor[S]): InstantGroupFrame[S] =
    Impl(group, transport)
}

trait InstantGroupFrame[S <: Sys[S]] {
  def component: Frame
  def group(implicit tx: S#Tx): Document.Group[S]
  def transport /* ( implicit tx: S#Tx ) */ : Document.Transport[S]
}
