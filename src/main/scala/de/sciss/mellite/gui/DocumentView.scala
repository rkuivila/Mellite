package de.sciss.mellite
package gui

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.stm.Disposable

trait DocumentView[S <: Sys[S]] extends Disposable[S#Tx] {
  def document: Document[S]
}
