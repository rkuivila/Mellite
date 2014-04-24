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

import de.sciss.synth.proc.{ArtifactLocationElem, Obj, Folder, Artifact}
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
  //  case class Select(elem: )

  def query[S <: Sys[S]](
         document: Document[S], file: File,
         folder: Option[stm.Source[S#Tx, Obj.T[S, Folder]]] = None,
         window: Option[desktop.Window] = None)
        (implicit cursor: stm.Cursor[S]): Option[stm.Source[S#Tx, Obj.T[S, ArtifactLocationElem]]] = {

    type LocSource = stm.Source[S#Tx, ArtifactLocationElem[S]]

    val options = cursor.step { implicit tx =>
      /* @tailrec */ def loop(xs: List[Obj[S]], res: Vec[Labeled[LocSource]]): Vec[Labeled[LocSource]] =
        xs match {
          case head :: tail =>
            val res1 = head.elem match {
              case a: ArtifactLocationElem[S] =>
                val parent = a.peer.directory
                if (Try(Artifact.relativize(parent, file)).isSuccess) {
                  res :+ Labeled(tx.newHandle(a))(head.attr.name)
                } else res

              case f: Folder[S] =>
                loop(f.peer.iterator.toList, res)

              case _ => res
            }
            loop(tail, res1)
          case _ :: tail  => loop(tail, res)
          case Nil        => res
      }

      val _options = loop(document.root.peer.iterator.toList, Vector.empty)
      _options
    }

    def createNew() = {
      queryNew(child = Some(file), window = window).map { case (dir, name) =>
        cursor.step { implicit tx =>
          val loc = create(dir, name, folder.map(_.apply().elem).getOrElse(document.root))
          tx.newHandle(loc)
        }
      }
    }

    options match {
      case Vec() => createNew()
      case Vec(Labeled(source)) => Some(source)

      case _ =>
        val ggList = new swing.ListView(options)
        ggList.selection.intervalMode = swing.ListView.IntervalMode.Single
        ggList.selection.indices += 0
        val opt = OptionPane.apply(message = ggList, messageType = OptionPane.Message.Question,
          optionType = OptionPane.Options.OkCancel, entries = Seq("Ok", "New Location", "Cancel"))
        opt.title = s"Choose Location for ${file.name}"
        val optRes = opt.show(window).id
        // println(s"res = $optRes, ok = ${OptionPane.Result.Ok.id}, cancel = ${OptionPane.Result.Cancel.id}")
        if (optRes == 0) {
          ggList.selection.items.headOption.map(_.value)
        } else if (optRes == 1) {
          createNew()
        } else {
          None
        }
    }
  }

  def queryNew(child: Option[File] = None, window: Option[Window] = None): Option[(File, String)] = {
    val dlg = FileDialog.folder(title = "Choose Artifact Base Location")
    dlg.show(None) match {
      case Some(dir) =>
        child match {
          case Some(file) if Try(Artifact.relativize(dir, file)).isFailure => queryNew(child, window) // try again
          case _ =>
            val res = Dialog.showInput[String](null,
              "Enter initial store name:", "New Artifact Location",
              Dialog.Message.Question, initial = dir.name)
            res.map(dir -> _)
        }
      case _=> None
    }
  }

  def create[S <: Sys[S]](directory: File, name: String, parent: Folder[S])
                         (implicit tx: S#Tx): Obj.T[S, ArtifactLocationElem] = {
    val peer  = Artifact.Location.Modifiable[S](directory)
    val elem  = ArtifactLocationElem(peer)
    val obj   = Obj(elem)
    obj.attr.name = name
    parent.peer.addLast(obj)
    obj
  }
}