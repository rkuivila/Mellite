/*
 *  ElementActions.scala
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

import de.sciss.synth.proc
import de.sciss.synth.proc.{Artifact, Obj, ExprImplicits, ProcKeys, Folder, AudioGraphemeElem, Grapheme, StringElem}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.file._
import de.sciss.lucre.expr.{Double => DoubleEx, Long => LongEx}
import proc.Implicits._
import de.sciss.lucre.event.Sys

object ObjectActions {
  def addAudioFile[S <: Sys[S]](folder: Folder[S], index: Int, loc: Artifact.Location.Modifiable[S],
                                f: File, spec: AudioFileSpec)
                               (implicit tx: S#Tx): Obj.T[S, AudioGraphemeElem] = {
    val imp       = ExprImplicits[S]
    import imp._
    val offset    = LongEx  .newVar[S](0L )
    val gain      = DoubleEx.newVar[S](1.0)
    val artifact  = loc.add(f)
    val audio     = Grapheme.Elem.Audio(artifact, spec, offset, gain)
    val name      = f.base
    val elem      = AudioGraphemeElem[S](audio)
    val obj       = Obj(elem)
    obj.attr.name = name
    if (index == -1) folder.peer.addLast(obj) else folder.peer.insert(index, obj)
    obj
  }

  def findAudioFile[S <: Sys[S]](root: Folder[S], file: File)
                                (implicit tx: S#Tx): Option[Obj.T[S, AudioGraphemeElem]] = {
    def loop(folder: Folder[S]): Option[Obj.T[S, AudioGraphemeElem]] =
      folder.peer.iterator.flatMap {
        case a: AudioGraphemeElem[S] if a.peer.value.artifact == file => Some(a.asInstanceOf[Obj.T[S, AudioGraphemeElem]])
        case f: Folder           [S] => loop(f)
        case _ => None
      } .toList.headOption

    loop(root)
  }
}