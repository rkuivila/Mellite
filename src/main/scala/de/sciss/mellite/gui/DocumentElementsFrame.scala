/*
 *  DocumentElementsFrame.scala
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

import impl.document.{ElementsFrameImpl => Impl}
import lucre.stm
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys

object DocumentElementsFrame {
  def apply[S <: Sys[S]](doc: Document[S])(implicit tx: S#Tx, cursor: stm.Cursor[S]): DocumentElementsFrame[S] =
    Impl(doc)
}

trait DocumentElementsFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
