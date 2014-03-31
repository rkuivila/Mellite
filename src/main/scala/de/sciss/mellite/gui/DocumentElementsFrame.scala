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
import de.sciss.lucre.expr.Expr

object DocumentElementsFrame {
  def apply[S <: Sys[S], S1 <: Sys[S1]](doc: Document[S], name: Option[Expr[S1, String]])
                        (implicit tx: S#Tx, cursor: stm.Cursor[S], bridge: S#Tx => S1#Tx): DocumentElementsFrame[S] =
    Impl(doc, nameOpt = name)
}

trait DocumentElementsFrame[S <: Sys[S]] extends Disposable[S#Tx] {
  def component: desktop.Window
  def document : Document[S]
}
