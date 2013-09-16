/*
 *  AudioFileView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2013 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
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