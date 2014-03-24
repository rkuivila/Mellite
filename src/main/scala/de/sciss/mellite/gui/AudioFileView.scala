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

import de.sciss.synth.proc.AuralSystem
import swing.Component
import Element.AudioGrapheme
import impl.audiofile.{ViewImpl => Impl}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.synth.Sys

object AudioFileView {
  def apply[S <: Sys[S]](document: Document[S], element: AudioGrapheme[S])
                        (implicit tx: S#Tx, aural: AuralSystem): AudioFileView[S] =
    Impl(document, element)
}
trait AudioFileView[S <: Sys[S]] extends Disposable[S#Tx] {
  def document: Document[S]
  def component: Component
  def element(implicit tx: S#Tx): AudioGrapheme[S]
}