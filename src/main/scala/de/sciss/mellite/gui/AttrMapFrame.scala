/*
 *  AttrMapFrame.scala
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

package de.sciss.mellite.gui

import de.sciss.lucre.stm
import de.sciss.synth.proc.Obj
import impl.document.{AttrMapFrameImpl => Impl}
import de.sciss.lucre.event.Sys
import de.sciss.lucre.swing.View
import de.sciss.desktop

object AttrMapFrame {
  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): AttrMapFrame[S] =
    Impl(obj)
}
trait AttrMapFrame[S <: Sys[S]] extends View[S] {
  def window  : desktop.Window
  def contents: AttrMapView[S]
}