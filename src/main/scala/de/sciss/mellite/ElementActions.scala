/*
 *  ElementActions.scala
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

import de.sciss.synth.proc.Artifact.Location
import de.sciss.synth.proc.{Grapheme, Sys}
import de.sciss.synth.expr.{Doubles, Longs}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.file._

object ElementActions {
  def addAudioFile[S <: Sys[S]](folder: Folder[S], index: Int, loc: Location.Modifiable[S],
                                f: File, spec: AudioFileSpec)
                               (implicit tx: S#Tx): Unit = {
    val offset    = Longs  .newVar[S](Longs  .newConst(0L))
    val gain      = Doubles.newVar[S](Doubles.newConst(1.0))
    val artifact  = loc.add(f)
    val audio     = Grapheme.Elem.Audio(artifact, spec, offset, gain)
    val name      = f.base
    val elem      = Element.AudioGrapheme(name, audio)
    if (index == -1) folder.addLast(elem) else folder.insert(index, elem)
  }
}