/*
 *  ElementView.scala
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

package de.sciss
package mellite
package gui

import de.sciss.synth.proc.{Elem, AudioGraphemeElem, ProcGroupElem, Obj, ArtifactLocationElem, DoubleElem, IntElem, StringElem, Artifact, Grapheme, FolderElem}
import de.sciss.lucre.stm
import java.io.File
import javax.swing.Icon
import de.sciss.synth.proc
import de.sciss.lucre.event.Sys
import javax.swing.undo.UndoableEdit
import impl.{ObjViewImpl => Impl}
import scala.language.higherKinds

object ObjView {
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean}
  import mellite.{Code => _Code, Recursion => _Recursion}
  import proc.{Folder => _Folder}

  trait Factory {
    def prefix: _String
    def icon  : Icon
    def typeID: _Int

    type E[~ <: Sys[~]] <: Elem[~]

    def apply[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ObjView[S]

    // type Init

    def initDialog[S <: Sys[S]](parentH: stm.Source[S#Tx, _Folder[S]], window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit]

    // def newInstance[S <: Sys[S]](init: Init)(implicit tx: S#Tx): Obj.T[S, E]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ObjView[S] = Impl(obj)

  val String: Factory { type E[S <: Sys[S]] = StringElem[S] /* ; type Init = (_String, _String) */ } = Impl.String
  trait String[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, StringElem]]
  }

  val Int: Factory { type E[S <: Sys[S]] = IntElem[S] /* ; type Init = (_String, _Int) */ } = Impl.Int
  trait Int[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, IntElem]]
    def value: _Int
  }

  val Double: Factory { type E[S <: Sys[S]] = DoubleElem[S] /* ; type Init = (_String, _Double) */ } = Impl.Double
  trait Double[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]]
  }

  val AudioGrapheme: Factory { type E[S <: Sys[S]] = AudioGraphemeElem[S] /* ; type Init = File */ } =
    Impl.AudioGrapheme

  trait AudioGrapheme[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]]
    def value: Grapheme.Value.Audio
  }

  val ArtifactLocation: Factory { type E[S <: Sys[S]] = ArtifactLocationElem[S] /* ; type Init = File */ } =
    Impl.ArtifactLocation

  trait ArtifactLocation[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, ArtifactLocationElem]]
    def directory: File
  }

  val Recursion: Factory { type E[S <: Sys[S]] = mellite.Recursion.Elem[S] /* ; type Init = Unit */ } = Impl.Recursion
  trait Recursion[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, mellite.Recursion.Elem]]
    def deployed: File
  }

  val Folder: Factory { type E[S <: Sys[S]] = FolderElem[S] /* ; type Init = _String */ } = Impl.Folder
  trait Folder[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, FolderElem]]
  }

  val ProcGroup: Factory { type E[S <: Sys[S]] = ProcGroupElem[S] /* ; type Init = _String */ } = Impl.ProcGroup
  trait ProcGroup[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, ProcGroupElem]]
  }

  val Code: Factory { type E[S <: Sys[S]] = _Code.Elem[S] } = Impl.Code
  trait Code[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, mellite.Code.Elem]]
    def value: _Code
  }
}
trait ObjView[S <: Sys[S]] {
  var name: String
  def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean
  def isEditable: Boolean

  def obj: stm.Source[S#Tx, Obj[S]]

  def prefix: String
  def icon  : Icon

  def value: Any
  def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit]
}