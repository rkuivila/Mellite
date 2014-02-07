/*
 *  DocumentView.scala
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

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm.Disposable

trait DocumentView[S <: Sys[S]] extends Disposable[S#Tx] {
  def document: Document[S]
}
