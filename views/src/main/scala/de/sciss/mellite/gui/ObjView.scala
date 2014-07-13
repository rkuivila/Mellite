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

import de.sciss.synth.proc.{BooleanElem, Elem, FolderElem, AudioGraphemeElem, Obj, DoubleElem, IntElem, StringElem, Grapheme}
import de.sciss.lucre.stm
import java.io.File
import javax.swing.Icon
import de.sciss.synth.proc
import de.sciss.lucre.synth.Sys
import javax.swing.undo.UndoableEdit
import impl.{ObjViewImpl => Impl}
import scala.language.higherKinds
import de.sciss.lucre.swing.Window
import de.sciss.lucre.{event => evt}
import scala.swing.{Component, Label}
import scala.collection.immutable.{IndexedSeq => Vec}
import scala.collection.breakOut

object ObjView {
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean}
  import mellite.{Code => _Code, Recursion => _Recursion, Action => _Action}
  import proc.{Folder => _Folder, ArtifactLocation => _ArtifactLocation, Proc => _Proc, Timeline => _Timeline, FadeSpec => _FadeSpec}

  //  final case class SelectionDrag[S <: Sys[S]](workspace: Workspace[S], selection: Vec[ObjView[S]]) {
  //    lazy val types: Set[_Int] = selection.map(_.typeID)(breakOut)
  //  }

  final case class Drag[S <: Sys[S]](workspace: Workspace[S], view: ObjView[S])

  // Document not serializable -- local JVM only DnD -- cf. stackoverflow #10484344
  // val SelectionFlavor = DragAndDrop.internalFlavor[SelectionDnDData[_]]
  val Flavor = DragAndDrop.internalFlavor[Drag[_]]

  trait Factory {
    def prefix: _String
    def icon  : Icon
    def typeID: _Int

    type E[~ <: evt.Sys[~]] <: Elem[~]

    def apply[S <: Sys[S]](obj: Obj.T[S, E])(implicit tx: S#Tx): ObjView[S]

    def initDialog[S <: Sys[S]](workspace: Workspace[S], parentH: stm.Source[S#Tx, _Folder[S]],
                                window: Option[desktop.Window])
                               (implicit cursor: stm.Cursor[S]): Option[UndoableEdit]
  }

  def addFactory(f: Factory): Unit = Impl.addFactory(f)

  def factories: Iterable[Factory] = Impl.factories

  def apply[S <: Sys[S]](obj: Obj[S])(implicit tx: S#Tx): ObjView[S] = Impl(obj)

  val String: Factory { type E[S <: evt.Sys[S]] = StringElem[S] } = Impl.String
  trait String[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, StringElem]]
  }

  val Int: Factory { type E[S <: evt.Sys[S]] = IntElem[S] } = Impl.Int
  trait Int[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, IntElem]]
    def value: _Int
  }

  val Double: Factory { type E[S <: evt.Sys[S]] = DoubleElem[S] } = Impl.Double
  trait Double[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]]
  }

  val Boolean: Factory { type E[S <: evt.Sys[S]] = BooleanElem[S] } = Impl.Boolean
  trait Boolean[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, BooleanElem]]
    def value: _Boolean
  }

  val AudioGrapheme: Factory { type E[S <: evt.Sys[S]] = AudioGraphemeElem[S] } =
    Impl.AudioGrapheme

  trait AudioGrapheme[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]]
    def value: Grapheme.Value.Audio
  }

  val ArtifactLocation: Factory { type E[S <: evt.Sys[S]] = _ArtifactLocation.Elem[S] } =
    Impl.ArtifactLocation

  trait ArtifactLocation[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, _ArtifactLocation.Elem]]
    def directory: File
  }

  val Recursion: Factory { type E[S <: evt.Sys[S]] = mellite.Recursion.Elem[S] } = Impl.Recursion
  trait Recursion[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, mellite.Recursion.Elem]]
    def deployed: File
  }

  val Folder: Factory { type E[S <: evt.Sys[S]] = FolderElem[S] } = Impl.Folder
  trait Folder[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, FolderElem]]
  }

  val Proc: Factory { type E[S <: evt.Sys[S]] = _Proc.Elem[S] } = Impl.Proc
  trait Proc[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, _Proc.Obj[S]]
  }

  val Timeline: Factory { type E[S <: evt.Sys[S]] = _Timeline.Elem[S] } = Impl.Timeline
  trait Timeline[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, _Timeline.Obj[S]]
  }

  val Code: Factory { type E[S <: evt.Sys[S]] = _Code.Elem[S] } = Impl.Code
  trait Code[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, _Code.Obj[S]]
    def value: _Code
  }

  val FadeSpec: Factory { type E[S <: evt.Sys[S]] = _FadeSpec.Elem[S] } = Impl.FadeSpec
  trait FadeSpec[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, _FadeSpec.Elem]]
    def value: _FadeSpec
  }

  val Action: Factory { type E[S <: evt.Sys[S]] = _Action.Elem[S] } = Impl.Action
  trait Action[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, _Action.Obj[S]]
    // def value: _Action
  }
}
trait ObjView[S <: Sys[S]] {
  def typeID: Int

  /** The contents of the `"name"` attribute of the object. This is directly
    * set by the table tree view. The object view itself must only make sure that
    * an initial value is provided.
    */
  var name: String

  /** The prefix is the type of object represented. For example, `"Int"` for an `Obj.T[S, IntElem]`, etc. */
  def prefix: String

  /** A view must provide an icon for the user interface. It should have a dimension of 32 x 32 and ideally
    * be drawn as vector graphics in order to look good when applying scaling.
    */
  def icon  : Icon

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Obj[S]]

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

  /** Whether a dedicated view/editor window exists for this type of object. */
  def isViewable: Boolean

  /** If the object is viewable, this method is invoked when the user pressed the eye button.
    * The method should return an appropriate view for this object, or `None` if no editor or viewer window
    * can be produced.
    *
    * TODO: should have optional window argument
    */
  def openView()(implicit tx: S#Tx, workspace: Workspace[S], cursor: stm.Cursor[S]): Option[Window[S]]
}