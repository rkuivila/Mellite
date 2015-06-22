/*
 *  ElementActions.scala
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

import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.synth.proc
import de.sciss.synth.proc.{Obj, FolderElem, ExprImplicits, Folder, AudioGraphemeElem, Grapheme}
import de.sciss.synth.io.AudioFileSpec
import de.sciss.file._
import de.sciss.lucre.expr.{Double => DoubleEx, Long => LongEx}
import org.scalautils.TypeCheckedTripleEquals
import proc.Implicits._
import de.sciss.lucre.event.Sys

object ObjectActions {
  def mkAudioFile[S <: Sys[S]](loc: ArtifactLocation.Modifiable[S], f: File, spec: AudioFileSpec)
                              (implicit tx: S#Tx): Obj.T[S, AudioGraphemeElem] = {
    val imp       = ExprImplicits[S]
    import imp._
    val offset    = LongEx  .newVar[S](0L )
    val gain      = DoubleEx.newVar[S](1.0)
    val artifact  = loc.add(f)
    val audio     = Grapheme.Expr.Audio(artifact, spec, offset, gain)
    val name      = f.base
    val elem      = AudioGraphemeElem[S](audio)
    val obj       = Obj(elem)
    obj.name = name
    // if (index == -1) folder.addLast(obj) else folder.insert(index, obj)
    obj
  }

  def findAudioFile[S <: Sys[S]](root: Folder[S], file: File)
                                (implicit tx: S#Tx): Option[Obj.T[S, AudioGraphemeElem]] = {
    def loop(folder: Folder[S]): Option[Obj.T[S, AudioGraphemeElem]] = {
      import TypeCheckedTripleEquals._
      folder.iterator.flatMap {
        case AudioGraphemeElem.Obj(objT) if objT.elem.peer.value.artifact === file => Some(objT)
        case FolderElem.Obj(objT) => loop(objT.elem.peer)
        case _ => None
      } .toList.headOption
    }

    loop(root)
  }
}