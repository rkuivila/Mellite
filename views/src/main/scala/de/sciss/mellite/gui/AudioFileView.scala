/*
 *  AudioFileView.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2015 Hanns Holger Rutz. All rights reserved.
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
import de.sciss.file.File
import de.sciss.lucre.swing.View
import de.sciss.lucre.stm

object AudioFileView {
  def apply[S <: Sys[S]](obj: Obj.T[S, AudioGraphemeElem])
                        (implicit tx: S#Tx, document: Workspace[S], cursor: stm.Cursor[S],
                         aural: AuralSystem): AudioFileView[S] =
    Impl(obj)
}
trait AudioFileView[S <: Sys[S]] extends ViewHasWorkspace[S] /* Disposable[S#Tx] */ {
  // def document: File // Document[S]
  // def component: Component
  def obj(implicit tx: S#Tx): Obj.T[S, AudioGraphemeElem]
}