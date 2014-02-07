/*
 *  DocumentViewHandler.scala
 *  (Mellite)
 *
 *  Copyright (c) 2012-2014 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is published under the GNU General Public License v2+
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.mellite
package gui

import de.sciss.lucre.synth.Sys
import collection.immutable.{IndexedSeq => Vec}

object DocumentViewHandler {
  lazy val instance: DocumentViewHandler = new DocumentViewHandler {
    private var map = Map.empty[Document[_], Vec[DocumentView[_]]] withDefaultValue Vec.empty

    def apply[S <: Sys[S]](document: Document[S]): Iterator[DocumentView[S]] = {
      requireEDT()
      map(document).iterator.asInstanceOf[Iterator[DocumentView[S]]]
    }

    def add[S <: Sys[S]](view: DocumentView[S]): Unit = {
      requireEDT()
      map += view.document -> (map(view.document) :+ view)
    }

    def remove[S <: Sys[S]](view: DocumentView[S]): Unit = {
      requireEDT()
      val vec = map(view.document)
      val idx = vec.indexOf(view)
      require(idx >= 0, s"View $view was not registered")
      map += view.document -> vec.patch(idx, Vec.empty, 1)
    }
  }
}
trait DocumentViewHandler /* extends Model... */ {
  def apply [S <: Sys[S]](document: Document[S]): Iterator[DocumentView[S]]
  def add   [S <: Sys[S]](view: DocumentView[S]): Unit
  def remove[S <: Sys[S]](view: DocumentView[S]): Unit
}