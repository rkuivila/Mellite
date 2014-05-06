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

import de.sciss.synth.proc.{AudioGraphemeElem, ProcGroupElem, Obj, ArtifactLocationElem, DoubleElem, IntElem, StringElem, ProcKeys, Elem, ExprImplicits, Artifact, Grapheme, FolderElem}
import de.sciss.lucre.{expr, stm}
import swing.Swing
import collection.immutable.{IndexedSeq => Vec}
import java.io.File
import java.awt.Toolkit
import javax.swing.{Icon, ImageIcon}
import scala.util.Try
import de.sciss.model.Change
import de.sciss.lucre.swing._
import de.sciss.synth.proc
import proc.Implicits._
import de.sciss.lucre.expr.Expr
import de.sciss.lucre.event.Sys
import de.sciss.icons.raphael
import java.awt.geom.Path2D

object ObjView {
  import java.lang.{String => _String}
  import scala.{Int => _Int, Double => _Double, Boolean => _Boolean}
  import mellite.{Code => _Code, Recursion => _Recursion}
  import proc.{Folder => _Folder}

  private[gui] def apply[S <: Sys[S]](/* parent: FolderLike[S], */ obj: Obj[S])
                                     (implicit tx: S#Tx): ObjView[S] = {
    // val name = obj.name.value
    val name: _String = obj.attr.name
    obj match {
      case IntElem.Obj(objT) =>
        val ex          = objT.elem.peer
        val value       = ex.value
        val isEditable  = ex match {
          case Expr.Var(_)  => true
          case _            => false
        }
        new Int.Impl(/* parent, */ tx.newHandle(objT), name, value, isEditable = isEditable)
      case DoubleElem.Obj(objT) =>
        val ex          = objT.elem.peer
        val value       = ex.value
        val isEditable  = ex match {
          case Expr.Var(_)  => true
          case _            => false
        }
        new Double.Impl(/* parent, */ tx.newHandle(objT), name, value, isEditable = isEditable)
      case StringElem.Obj(objT) =>
        val ex          = objT.elem.peer
        val value       = ex.value
        val isEditable  = ex match {
          case Expr.Var(_)  => true
          case _            => false
        }
        new String.Impl(/* parent, */ tx.newHandle(objT), name, value, isEditable = isEditable)
      case FolderElem.Obj(objT) =>
        val res = new Folder.Impl[S](/* parent, */ tx.newHandle(objT), name)
        // res.children = e.entity.iterator.map(apply(res, _)(tx)).toIndexedSeq
        res
      case ProcGroupElem.Obj(objT) =>
        new ProcGroup.Impl(/* parent, */ tx.newHandle(objT), name)
      case AudioGraphemeElem.Obj(objT) =>
        val value = objT.elem.peer.value
        new AudioGrapheme.Impl(/* parent, */ tx.newHandle(objT), name, value)
      case ArtifactLocationElem.Obj(objT) =>
        val value = objT.elem.peer.directory
        new ArtifactLocation.Impl(/* parent, */ tx.newHandle(objT), name, value)
      case _Recursion.Elem.Obj(objT) =>
        val value = objT.elem.peer.deployed.elem.peer.artifact.value
        new Recursion.Impl(/* parent, */ tx.newHandle(objT), name, value)
      case _Code.Elem.Obj(objT) =>
        val value = objT.elem.peer.value
        new Code.Impl(/* parent, */ tx.newHandle(objT), name, value)
    }
  }

  // -------- String --------

  object String {
    private val icon = raphaelIcon(raphael.Shapes.Font)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, StringElem]],
                                                   var name: _String, var value: _String,
                                                   override val isEditable: _Boolean)
      extends String[S] with ObjView.Impl[S] {

      def prefix = "String"
      def icon = String.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): _Boolean = value match {
        case s: _String =>
          obj().elem.peer match {
            case Expr.Var(vr) =>
              vr() match {
                case Expr.Const(x) if x == s => false
                case _ =>
                  val imp = ExprImplicits[S]
                  import imp._
                  vr() = s
                  true
              }

            case _ => false
          }

        case _ => false
      }

      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: _String) =>
          deferTx { value = now }
          true
        case _ => false
      }
    }
  }
  sealed trait String[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, StringElem]]
  }

  // -------- Int --------

  object Int {
    private val icon = raphaelIcon(Shapes.IntegerNumbers)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, IntElem]],
                                                   var name: _String, var value: _Int,
                                                   override val isEditable: _Boolean)
      extends Int[S] with ObjView.Impl[S] {

      def prefix = "Int"
      def icon   = Int.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): _Boolean = {
        val numOpt: Option[_Int] = value match {
          case num: _Int  => Some(num)
          case s: _String => Try(s.toInt).toOption
        }
        numOpt.exists { num =>
          obj().elem.peer match {
            case Expr.Var(vr) =>
              vr() match {
                case Expr.Const(x) if x != num =>
                  val imp = ExprImplicits[S]
                  import imp._
                  vr() = num
                  true

                case _ => false
              }

            case _ => false
          }
        }
      }

      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: _Int) =>
          deferTx { value = now }
          true
        case _ => false
      }
    }
  }
  sealed trait Int[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, IntElem]]
    var value: _Int
  }

  // -------- Double --------

  object Double {
    private val icon = raphaelIcon(Shapes.RealNumbers)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]],
                                                   var name: _String, var value: _Double,
                                                   override val isEditable: _Boolean)
      extends Double[S] with ObjView.Impl[S] {

      def prefix = "Double"
      def icon = Double.icon

      def tryUpdate(value: Any)(implicit tx: S#Tx): _Boolean = {
        val numOpt = value match {
          case num: _Double => Some(num)
          case s: _String => Try(s.toDouble).toOption
        }
        // XXX TODO: DRY (see Int)
        numOpt.exists {
          num =>
            obj().elem.peer match {
              case Expr.Var(vr) =>
                vr() match {
                  case Expr.Const(x) if x != num =>
                    val imp = ExprImplicits[S]
                    import imp._
                    vr() = num
                    true

                  case _ => false
                }

              case _ => false
            }
        }
      }

      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: _Double) =>
          deferTx { value = now }
          true
        case _ => false
      }
    }
  }
  sealed trait Double[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, DoubleElem]]
  }

  //  // -------- FolderLike --------
  //
  //  sealed trait FolderLike[S <: Sys[S]] extends Renderer[S] {
  //    //    def branchID(implicit tx: S#Tx) = folder().id
  //    //
  //    //    def folder: stm.Source[S#Tx, _Folder[S]]
  //
  //    //    def tryConvert(upd: Any): _Folder.Changes[S] = upd match {
  //    //      case ll: expr.List.Update[_, _, _] =>
  //    //        convert(ll.asInstanceOf[expr.List.Update[S, Obj[S], Obj.Update[S]]])
  //    //      case _ => Vector.empty
  //    //    }
  //    //
  //    //    def convert(upd: expr.List.Update[S, Obj[S], Obj.Update[S]]): _Folder.Changes[S] =
  //    //      upd.changes.map {
  //    //        case expr.List.Added  (idx, obj)  => _Folder.Added  (idx, obj)
  //    //        case expr.List.Removed(idx, obj)  => _Folder.Removed(idx, obj)
  //    //        case expr.List.Element(obj, ou)   => _Folder.Element(obj, ou)
  //    //      }
  //    //
  //    //    /** The children of the folder. This variable _must only be accessed or updated_ on the event thread. */
  //    //    var children: Vec[ObjView[S]]
  //
  //    def value = ()
  //  }

  // -------- Group --------

  object Folder {
    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, FolderElem]],
                                                   var name: _String)
      extends Folder[S] with ObjView.Impl[S] {

      def value = ()

      //      var children = Vec.empty[ObjView[S]]
      //
      //      // def folder(implicit tx: S#Tx): _Folder[S] = obj().elem.peer
      //      def folder: stm.Source[S#Tx, _Folder[S]] = stm.Source.map(obj)(_.elem.peer)

      def prefix = "Folder"
      def icon = Swing.EmptyIcon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): _Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = false  // element addition and removal is handled by folder view
    }
  }
  sealed trait Folder[S <: Sys[S]] extends ObjView[S] /* Branch[S] */ {
    def obj: stm.Source[S#Tx, Obj.T[S, FolderElem]]
  }

  // -------- ProcGroup --------

  object ProcGroup {
    private val icon = raphaelIcon(raphael.Shapes.Ruler)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, ProcGroupElem]],
                                                   var name: _String)
      extends ProcGroup[S] with ObjView.Impl[S] {

      def prefix = "ProcGroup"
      def value = ()
      def icon = ProcGroup.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): _Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = false
    }
  }
  sealed trait ProcGroup[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, ProcGroupElem]]
  }

  // -------- AudioGrapheme --------

  object AudioGrapheme {
    val icon: Icon = raphaelIcon(raphael.Shapes.Music)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]],
                                                   var name: _String, var value: Grapheme.Value.Audio)
      extends AudioGrapheme[S] with ObjView.Impl[S] {

      def prefix = "String"
      def icon = AudioGrapheme.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): _Boolean = false

      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Change(_, now: Grapheme.Value.Audio) =>
          deferTx { value = now }
          true
        case _ => false
      }
    }
  }
  sealed trait AudioGrapheme[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, AudioGraphemeElem]]
    var value: Grapheme.Value.Audio
  }

  // -------- ArtifactLocation --------

  object ArtifactLocation {
    private val icon = raphaelIcon(raphael.Shapes.Location)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, ArtifactLocationElem]],
                                                   var name: _String, var directory: File)
      extends ArtifactLocation[S] with ObjView.Impl[S] {

      def prefix  = "ArtifactStore"
      def value   = directory
      def icon    = ArtifactLocation.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): _Boolean = false

      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = update match {
        case Artifact.Location.Moved(_, Change(_, now)) =>
          deferTx { directory = now }
          true
        case _ => false
      }
    }
  }
  sealed trait ArtifactLocation[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, ArtifactLocationElem]]
    var directory: File
  }

  // -------- Recursion --------

  object Recursion {
    private val icon = raphaelIcon(raphael.Shapes.Quote)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, mellite.Recursion.Elem]],
                                                   var name: _String, var deployed: File)
      extends Recursion[S] with ObjView.Impl[S] {

      def prefix  = "Recursion"
      def value   = deployed
      def icon    = Recursion.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): _Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = false
    }
  }
  sealed trait Recursion[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, mellite.Recursion.Elem]]
    var deployed: File
  }

  // -------- ArtifactLocation --------

  object Code {
    private val icon = raphaelIcon(raphael.Shapes.Code)

    private[ObjView] final class Impl[S <: Sys[S]](val obj: stm.Source[S#Tx, Obj.T[S, mellite.Code.Elem]],
                                                   var name: _String, var value: _Code)
      extends Code[S] with ObjView.Impl[S] {

      def prefix  = "Code"
      def icon    = Code.icon

      def tryUpdate  (value : Any)(implicit tx: S#Tx): _Boolean = false
      def checkUpdate(update: Any)(implicit tx: S#Tx): _Boolean = false
    }
  }
  sealed trait Code[S <: Sys[S]] extends ObjView[S] {
    def obj: stm.Source[S#Tx, Obj.T[S, mellite.Code.Elem]]
    var value: _Code
  }

  // -------- Root --------

  //  object Root {
  //    private[gui] def apply[S <: Sys[S]](folder: _Folder[S])(implicit tx: S#Tx): Root[S] = {
  //      import _Folder.serializer
  //      val res = new Impl(tx.newHandle(folder))
  //      // res.children = folder.iterator.map(ElementView(res, _)(tx)).toIndexedSeq
  //      res
  //    }
  //
  //    private final class Impl[S <: Sys[S]](val folder: stm.Source[S#Tx, _Folder[S]])
  //      extends Root[S] {
  //
  //      def name = "root"
  //      def icon = Swing.EmptyIcon
  //
  //      def tryUpdate(value: Any)(implicit tx: S#Tx): _Boolean = false
  //
  //      override def toString = "ElementView.Root"
  //    }
  //  }
  //  sealed trait Root[S <: Sys[S]] extends FolderLike[S]

  private def raphaelIcon(shape: Path2D => Unit): Icon = raphael.Icon(16)(shape)

  private sealed trait Impl[S <: Sys[S]] extends ObjView[S] {
    protected def prefix: _String
    override def toString = s"ElementView.$prefix(name = $name)"
  }

  //  sealed trait Renderer[S <: Sys[S]] {
  //    // private[gui] def componentFor(tree: Tree[_], info: Tree.Renderer.CellInfo): Component
  //    def name: _String
  //    def value: Any
  //    def tryUpdate(value: Any)(implicit tx: S#Tx): _Boolean
  //    def icon: Icon
  //  }
}
/* sealed */ trait ObjView[S <: Sys[S]] /* extends ObjView.Renderer[S] */ {
  var name: String
  def checkUpdate(update: Any)(implicit tx: S#Tx): Boolean
  def isEditable: Boolean = false

  def obj: stm.Source[S#Tx, Obj[S]]

  // formerly Renderer
  def value: Any
  def tryUpdate(value: Any)(implicit tx: S#Tx): Boolean
  def icon: Icon
}