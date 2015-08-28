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

import de.sciss.lucre.stm
import de.sciss.lucre.synth.Sys
import de.sciss.mellite.gui.impl.audiofile.{ViewImpl => Impl}
import de.sciss.synth.proc.{AuralSystem, Grapheme}

object AudioFileView {
  def apply[S <: Sys[S]](obj: Grapheme.Expr.Audio[S])
                        (implicit tx: S#Tx, document: Workspace[S], cursor: stm.Cursor[S],
                         aural: AuralSystem): AudioFileView[S] =
    Impl(obj)
}
trait AudioFileView[S <: Sys[S]] extends ViewHasWorkspace[S] /* Disposable[S#Tx] */ {
  // def document: File // Document[S]
  // def component: Component
  def obj(implicit tx: S#Tx): Grapheme.Expr.Audio[S]
}