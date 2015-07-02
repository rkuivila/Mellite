/*
 *  ElementView.scala
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

package de.sciss
package mellite
package gui

import javax.swing.Icon

import de.sciss.lucre.{event => evt, stm}
import de.sciss.lucre.stm.Disposable
import de.sciss.lucre.swing.Window
import de.sciss.lucre.synth.Sys
import de.sciss.synth.proc.{Elem, Obj}

import scala.language.higherKinds

object ObjView {
  trait Factory {
    def prefix: String
    def icon  : Icon
    def typeID: Int

    type Config[S <: evt.Sys[S]]

    type E[~ <: evt.Sys[~]] <: Elem[~]

    /** Whether it is possible to create an instance of the object via a GUI dialog. */
    def hasMakeDialog: Boolean

    /** Provides an optional initial configuration for the make-new-instance dialog. */
    def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                   (implicit cursor: stm.Cursor[S]): Option[Config[S]]

    /** Creates an object from a configuration.
      * The reason that the result type is not `Obj.T[S, E]` is
      * that we allow the returning of a number of auxiliary other objects as well.
      */
    def makeObj[S <: Sys[S]](config: Config[S])(implicit tx: S#Tx): List[Obj[S]]
  }
}
trait ObjView[S <: evt.Sys[S]] extends Disposable[S#Tx] {
  // type E[~ <: evt.Sys[~]] <: Elem[~]

  // def factory: ObjView.Factory

  /** The contents of the `"name"` attribute of the object. This is directly
    * set by the table tree view. The object view itself must only make sure that
    * an initial value is provided.
    */
  var nameOption: Option[String]

  /** Convenience method that returns an "unnamed" string if no name is set. */
  def name: String = nameOption.getOrElse(TimelineObjView.Unnamed)

  /** The prefix is the type of object represented. For example, `"Int"` for an `Obj.T[S, IntElem]`, etc. */
  def prefix: String

  /** A view must provide an icon for the user interface. It should have a dimension of 32 x 32 and ideally
    * be drawn as vector graphics in order to look good when applying scaling.
    */
  def icon  : Icon

  /** The view must store a handle to its underlying model. */
  def obj: stm.Source[S#Tx, Obj[S]] // Obj.T[S, E]]

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