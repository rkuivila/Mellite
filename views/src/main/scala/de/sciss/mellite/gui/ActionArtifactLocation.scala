/*
 *  ActionArtifactLocation.scala
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

import de.sciss.lucre.artifact.{ArtifactLocation, Artifact}
import de.sciss.synth.proc.{ArtifactLocationElem, Obj, Folder, FolderElem}
import de.sciss.lucre.stm
import collection.immutable.{IndexedSeq => Vec}
import scala.util.Try
import de.sciss.desktop.{OptionPane, Window, FileDialog}
import scala.swing.Dialog
import de.sciss.file._
import de.sciss.swingplus.Labeled
import de.sciss.synth.proc
import proc.Implicits._
import de.sciss.lucre.event.Sys

object ActionArtifactLocation {
  //  sealed trait QueryResult
  //  case class Cancel extends QueryResult
  //  case clstm.Source[S#Tx, Folder[S]]ass Select(elem: )

  // case class NewConfig(name: String, directory: File)

  //  def queryOrCreate[S <: Sys[S]](
  //                          root: stm.Source[S#Tx, Folder[S]], file: File,
  //                          folder: Option[stm.Source[S#Tx, Obj.T[S, FolderElem]]] = None,
  //                          window: Option[desktop.Window] = None)
  //                        (implicit cursor: stm.Cursor[S]): Option[stm.Source[S#Tx, Obj.T[S, ArtifactLocationElem]]] = {
  //    query(root = root, file = file, window = window).map { either =>
  //      either.fold(identity) { case (name, directory) => create(name = name, directory = directory) }
  //    }

  type LocationSource [S <: Sys[S]] = stm.Source[S#Tx, Obj.T[S, ArtifactLocationElem]]
  type QueryResult    [S <: Sys[S]] = Either[LocationSource[S], (String, File)]

  def merge[S <: Sys[S]](result: QueryResult[S])
                        (implicit tx: S#Tx): Option[(Option[Obj[S]], ArtifactLocation.Modifiable[S])] = {
    val (list0, loc) = result match {
      case Left(source) => (None, source())
      case Right((name, directory)) =>
        val locM = create(name, directory)
        (Some(locM), locM)
    }
    loc.elem.peer.modifiableOption.map(list0 -> _)
  }

  def query[S <: Sys[S]](
                          root: stm.Source[S#Tx, Folder[S]], file: File,
                          window: Option[desktop.Window] = None)
                        (implicit cursor: stm.Cursor[S]): Option[QueryResult[S]] = {

    def createNew(): Option[(String, File)] = queryNew(child = Some(file), window = window)

    val options = find(root = root, file = file, window = window)

    options match {
      case Vec() => createNew().map(Right.apply)
      case Vec(Labeled(source)) => Some(Left(source))

      case _ =>
        val ggList = new swingplus.ListView(options)
        ggList.selection.intervalMode = swingplus.ListView.IntervalMode.Single
        ggList.selection.indices += 0
        val opt = OptionPane.apply(message = ggList, messageType = OptionPane.Message.Question,
          optionType = OptionPane.Options.OkCancel, entries = Seq("Ok", "New Location", "Cancel"))
        opt.title = s"Choose Location for ${file.name}"
        val optRes = opt.show(window).id
        // println(s"res = $optRes, ok = ${OptionPane.Result.Ok.id}, cancel = ${OptionPane.Result.Cancel.id}")
        if (optRes == 0) {
          ggList.selection.items.headOption.map(v => Left(v.value))
        } else if (optRes == 1) {
          createNew().map(Right.apply)
        } else {
          None
        }
    }
  }

  def find[S <: Sys[S]](
         root: stm.Source[S#Tx, Folder[S]], file: File,
         window: Option[desktop.Window] = None)
        (implicit cursor: stm.Cursor[S]): Vec[Labeled[LocationSource[S]]] = {

    val options: Vec[Labeled[LocationSource[S]]] = cursor.step { implicit tx =>
      /* @tailrec */ def loop(xs: List[Obj[S]], res: Vec[Labeled[LocationSource[S]]]): Vec[Labeled[LocationSource[S]]] =
        xs match {
          case head :: tail =>
            val res1 = head match {
              case ArtifactLocationElem.Obj(objT) =>
                val parent = objT.elem.peer.directory
                if (Try(Artifact.relativize(parent, file)).isSuccess) {
                  res :+ Labeled(tx.newHandle(objT))(objT.name)
                } else res

              case FolderElem.Obj(objT) =>
                loop(objT.elem.peer.iterator.toList, res)

              case _ => res
            }
            loop(tail, res1)

          case Nil => res
      }

      val _options = loop(root().iterator.toList, Vector.empty)
      _options
    }

    options
  }

  def queryNew(child: Option[File] = None, window: Option[Window] = None): Option[(String, File)] = {
    val dlg = FileDialog.folder(init = child.flatMap(_.parentOption), title = "Choose Artifact Base Location")
    dlg.show(None) match {
      case Some(dir) =>
        child match {
          case Some(file) if Try(Artifact.relativize(dir, file)).isFailure => queryNew(child, window) // try again
          case _ =>
            val res = Dialog.showInput[String](null,
              "Enter initial store name:", "New Artifact Location",
              Dialog.Message.Question, initial = dir.name)
            res.map(_ -> dir)
        }
      case _=> None
    }
  }

  def create[S <: Sys[S]](name: String, directory: File)(implicit tx: S#Tx): Obj.T[S, ArtifactLocationElem] = {
    val peer  = ArtifactLocation[S](directory)
    val elem  = ArtifactLocationElem(peer)
    val obj   = Obj(elem)
    obj.name = name
    obj
  }
}