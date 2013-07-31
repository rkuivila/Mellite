package de.sciss
package mellite
package gui

import de.sciss.synth.proc.{Artifact, Sys}
import de.sciss.lucre.stm
import de.sciss.mellite.Element.ArtifactLocation
import collection.immutable.{IndexedSeq => IIdxSeq}
import scala.util.Try
import de.sciss.desktop.{OptionPane, Window, FileDialog}
import scala.swing.Dialog
import de.sciss.file._
import de.sciss.swingplus.Labeled

object ActionArtifactLocation {
  //  sealed trait QueryResult
  //  case class Cancel extends QueryResult
  //  case class Select(elem: )

  def query[S <: Sys[S]](
         document: Document[S], file: File,
         folder: Option[stm.Source[S#Tx, Element.Folder[S]]] = None,
         window: Option[desktop.Window] = None)
        (implicit cursor: stm.Cursor[S]): Option[stm.Source[S#Tx, ArtifactLocation[S]]] = {

    type LocSource = stm.Source[S#Tx, ArtifactLocation[S]]

    val options = cursor.step { implicit tx =>
      /* @tailrec */ def loop(xs: List[Element[S]], res: IIdxSeq[Labeled[LocSource]]): IIdxSeq[Labeled[LocSource]] =
        xs match {
          case (a: ArtifactLocation[S]) :: tail =>
            val parent  = a.entity.directory
            val res1    = if (Try(Artifact.relativize(parent, file)).isSuccess) {
              res :+ Labeled(tx.newHandle(a))(a.name.value)
            } else res
            loop(tail, res1)
          case (f: Element.Folder[S]) :: tail =>
            val res1 = loop(f.entity.iterator.toList, res)
            loop(tail, res1)
          case _ :: tail  => loop(tail, res)
          case Nil        => res
      }

      val _options = loop(document.elements.iterator.toList, Vector.empty)
      _options
    }

    def createNew() = {
      queryNew(child = Some(file), window = window).map { case (dir, name) =>
        cursor.step { implicit tx =>
          val loc = create(dir, name, folder.map(_.apply().entity).getOrElse(document.elements))
          tx.newHandle(loc)
        }
      }
    }

    options match {
      case IIdxSeq() => createNew()
      case IIdxSeq(Labeled(source)) => Some(source)

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
                         (implicit tx: S#Tx): Element.ArtifactLocation[S] = {
    val res = Element.ArtifactLocation(name, Artifact.Location.Modifiable(directory))
    parent.addLast(res)
    res
  }
}