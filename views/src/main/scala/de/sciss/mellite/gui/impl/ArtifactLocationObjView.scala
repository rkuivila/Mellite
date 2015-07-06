/*
 *  ArtifactLocationObjView.scala
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
package impl

import javax.swing.undo.UndoableEdit

import de.sciss.desktop
import de.sciss.file._
import de.sciss.icons.raphael
import de.sciss.lucre.artifact.ArtifactLocation
import de.sciss.lucre.swing._
import de.sciss.lucre.synth.Sys
import de.sciss.lucre.{event => evt, stm}
import de.sciss.mellite.gui.edit.EditArtifactLocation
import de.sciss.model.Change
import de.sciss.synth.proc
import de.sciss.synth.proc.impl.ElemImpl
import de.sciss.synth.proc.{ArtifactLocationElem, Obj}
import org.scalautils.TypeCheckedTripleEquals

// -------- ArtifactLocation --------

object ArtifactLocationObjView extends ListObjView.Factory {
  type E[S <: evt.Sys[S]] = ArtifactLocationElem[S]
  val icon      = ObjViewImpl.raphaelIcon(raphael.Shapes.Location)
  val prefix    = "ArtifactLocation"
  def humanName = "File Location"
  def typeID    = ElemImpl.ArtifactLocation.typeID
  def hasMakeDialog = true

  def category = ObjView.categResources

  def mkListView[S <: Sys[S]](obj: ArtifactLocationElem.Obj[S])(implicit tx: S#Tx): ArtifactLocationObjView[S] with ListObjView[S] = {
    val peer      = obj.elem.peer
    val value     = peer.directory
    val editable  = peer.modifiableOption.isDefined
    new Impl(tx.newHandle(obj), value, isEditable = editable).initAttrs(obj)
  }

  type Config[S <: evt.Sys[S]] = ObjViewImpl.PrimitiveConfig[File]

  def initMakeDialog[S <: Sys[S]](workspace: Workspace[S], window: Option[desktop.Window])
                                 (implicit cursor: stm.Cursor[S]): Option[Config[S]] =
    ActionArtifactLocation.queryNew(window = window)

  def makeObj[S <: Sys[S]](config: (String, File))(implicit tx: S#Tx): List[Obj[S]] = {
    import proc.Implicits._
    val (name, directory) = config
    val peer  = ArtifactLocation[S](directory)
    val elem  = ArtifactLocationElem(peer)
    val obj   = Obj(elem)
    obj.name = name
    obj :: Nil
  }

  final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, ArtifactLocationElem.Obj[S]],
                                var directory: File, val isEditable: Boolean)
    extends ArtifactLocationObjView[S]
    with ListObjView /* .ArtifactLocation */[S]
    with ObjViewImpl.Impl[S]
    with ListObjViewImpl.StringRenderer
    with ObjViewImpl.NonViewable[S] {

    type E[~ <: evt.Sys[~]] = ArtifactLocationElem[~]

    def factory = ArtifactLocationObjView

    def value   = directory

    def isUpdateVisible(update: Any)(implicit tx: S#Tx): Boolean = update match {
      case ArtifactLocation.Moved(_, Change(_, now)) =>
        deferTx { directory = now }
        true
      case _ => false
    }

    def tryEdit(value: Any)(implicit tx: S#Tx, cursor: stm.Cursor[S]): Option[UndoableEdit] = {
      val dirOpt = value match {
        case s: String  => Some(file(s))
        case f: File    => Some(f)
        case _          => None
      }
      dirOpt.flatMap { newDir =>
        val loc = obj().elem.peer
        import TypeCheckedTripleEquals._
        if (loc.directory === newDir) None else loc.modifiableOption.map { mod =>
          EditArtifactLocation(mod, newDir)
        }
      }
    }
  }
}
trait ArtifactLocationObjView[S <: evt.Sys[S]] extends ObjView[S] {
  override def obj: stm.Source[S#Tx, ArtifactLocationElem.Obj[S]]
  def directory: File
}