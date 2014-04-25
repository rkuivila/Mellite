/*
 *  AudioFileView.scala
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

import de.sciss.synth.proc.{Obj, AudioGraphemeElem, AuralSystem}
import swing.Component
import impl.audiofile.{ViewImpl => Impl}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys

object AudioFileView {
  def apply[S <: Sys[S]](document: Document[S], obj: Obj.T[S, AudioGraphemeElem])
                        (implicit tx: S#Tx, aural: AuralSystem): AudioFileView[S] =
    Impl(document, obj)
}
trait AudioFileView[S <: Sys[S]] extends Disposable[S#Tx] {
  def document: Document[S]
  def component: Component
  def obj(implicit tx: S#Tx): Obj.T[S, AudioGraphemeElem]
}