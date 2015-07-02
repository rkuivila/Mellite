/*
 *  ListObjView.scala
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

import javax.swing.undo.UndoableEdit

import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite
import de.sciss.mellite.gui.impl.{ListObjViewImpl, ObjViewImpl}
import de.sciss.synth.proc.{ArtifactLocationElem, AudioGraphemeElem, BooleanElem, DoubleElem, FolderElem, IntElem, LongElem, Obj, StringElem}

import scala.language.higherKinds
import scala.swing.{Component, Label}

object ListObjView {
  import java.lang.{String => _String}

  import de.sciss.lucre.artifact.{ArtifactLocation => _ArtifactLocation}
  import de.sciss.nuages.{Nuages => _Nuages}
  import de.sciss.synth.proc.{Action => _Action, Code => _Code, Ensemble => _Ensemble, FadeSpec => _FadeSpec, Folder => _Folder, Proc => _Proc, Timeline => _Timeline}

  import scala.{Boolean => _Boolean, Double => _Double, Int => _Int, Long => _Long}

  //  final case class SelectionDrag[S <: Sys[S]](workspace: Workspace[S], selection: Vec[ObjView[S]]) {
  //    lazy val types: Set[_Int] = selection.map(_.typeID)(breakOut)
  //  }

  final case class Drag[S <: Sys[S]](workspace: Workspace[S], view: ObjView[S])

  // Document not serializable -- local JVM only DnD -- cf. stackoverflow #10484344
  // val SelectionFlavor = DragAndDrop.internalFlavor[SelectionDnDData[_]]
  val Flavor = DragAndDrop.internalFlavor[Drag[_]]

  trait Factory extends ObjView.Factory {
    def mkListView[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ListObjView[S]
  }

  def addFactory(f: Factory): Unit = ListObjViewImpl.addFactory(f)

  def factories: Iterable[Factory] = ListObjViewImpl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ObjView[S] = ListObjViewImpl(obj)

  val String: Factory { type E[S <: evt.Sys[S]] = StringElem[S] } = ObjViewImpl.String
//  trait String[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, StringElem]]
//  }

  val Int: Factory { type E[S <: evt.Sys[S]] = IntElem[S] } = ObjViewImpl.Int
//  trait Int[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, IntElem]]
//    def value: _Int
//  }

  val Long: Factory { type E[S <: evt.Sys[S]] = LongElem[S] } = ObjViewImpl.Long
//  trait Long[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, LongElem]]
//    def value: _Long
//  }

  val Double: Factory { type E[S <: evt.Sys[S]] = DoubleElem[S] } = ObjViewImpl.Double
//  trait Double[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]]
//  }

  val Boolean: Factory { type E[S <: evt.Sys[S]] = BooleanElem[S] } = ObjViewImpl.Boolean
//  trait Boolean[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, BooleanElem]]
//    def value: _Boolean
//  }

  val AudioGrapheme: Factory { type E[S <: evt.Sys[S]] = AudioGraphemeElem[S] } =
    ObjViewImpl.AudioGrapheme

//  trait AudioGrapheme[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]]
//    def value: Grapheme.Value.Audio
//  }

  val ArtifactLocation: Factory { type E[S <: evt.Sys[S]] = ArtifactLocationElem[S] } =
    ObjViewImpl.ArtifactLocation

//  trait ArtifactLocation[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, ArtifactLocationElem.Obj[S]]
//    def directory: File
//  }

//  trait Artifact[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, ArtifactElem.Obj[S]]
//    def file: File
//  }

  val Recursion: Factory { type E[S <: evt.Sys[S]] = mellite.Recursion.Elem[S] } = ObjViewImpl.Recursion
//  trait Recursion[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, mellite.Recursion.Elem]]
//    def deployed: File
//  }

  val Folder: Factory { type E[S <: evt.Sys[S]] = FolderElem[S] } = ObjViewImpl.Folder
//  trait Folder[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, FolderElem]]
//  }

  val Proc: Factory { type E[S <: evt.Sys[S]] = _Proc.Elem[S] } = ObjViewImpl.Proc
//  trait Proc[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, _Proc.Obj[S]]
//  }

  val Timeline: Factory { type E[S <: evt.Sys[S]] = _Timeline.Elem[S] } = ObjViewImpl.Timeline
//  trait Timeline[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, _Timeline.Obj[S]]
//  }

  val Code: Factory { type E[S <: evt.Sys[S]] = _Code.Elem[S] } = ObjViewImpl.Code
//  trait Code[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, _Code.Obj[S]]
//    def value: _Code
//  }

  val FadeSpec: Factory { type E[S <: evt.Sys[S]] = _FadeSpec.Elem[S] } = ObjViewImpl.FadeSpec
//  trait FadeSpec[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, Obj.T[S, _FadeSpec.Elem]]
//    def value: _FadeSpec
//  }

  val Action: Factory { type E[S <: evt.Sys[S]] = _Action.Elem[S] } = ObjViewImpl.Action
//  trait Action[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, _Action.Obj[S]]
//    // def value: _Action
//  }

  val Ensemble: Factory { type E[S <: evt.Sys[S]] = _Ensemble.Elem[S] } = ObjViewImpl.Ensemble
//  trait Ensemble[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, _Ensemble.Obj[S]]
//    def playing: _Boolean
//  }

  val Nuages: Factory { type E[S <: evt.Sys[S]] = _Nuages.Elem[S] } = ObjViewImpl.Nuages
//  trait Nuages[S <: Sys[S]] extends ObjView[S] {
//    def obj: stm.Source[S#Tx, _Nuages.Obj[S]]
//  }
}
trait ListObjView[S <: evt.Sys[S]] extends ObjView[S] {
  /** Passes in a received opaque update to ask whether the view should be repainted due to this update.
    * This is a transactional method. If the view wants to update its internal state, it should
    * do that using `deferTx` to perform mutable state changes on the EDT, and then return `true` to
    * trigger a refresh of the table row.
    */
  def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean

  /** The opaque view value passed into the renderer. */
  def value: Any

  /** Configures the value cell renderer. The simplest case would be
    * `label.text = value.toString`. In order to leave the cell blank, just return the label.
    * One can also set its icon.
    */
  def configureRenderer(label: Label): Component

  /** Whether the opaque value part of the view can be edited in-place (inside the table itself). */
  def isEditable: Boolean

  /** Given that the view is editable, this method is called when the editor gave notification about
    * the editing being done. It is then the duty of the view to issue a corresponding transactional
    * mutation, returned in an undoable edit. Views that do not support editing should just return `None`.
    */
  def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit]
}
