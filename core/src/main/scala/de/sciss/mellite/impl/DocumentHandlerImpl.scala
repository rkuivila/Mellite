/*
 *  DocumentHandlerImpl.scala
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

package de.sciss.mellite
package impl

import de.sciss.model.impl.ModelImpl
import de.sciss.lucre.event.Sys
import de.sciss.lucre.stm.{TxnLike, Disposable}
import scala.concurrent.stm.{Ref => STMRef, TMap}
import de.sciss.file._
import scala.collection.immutable.{IndexedSeq => Vec}

object DocumentHandlerImpl {
  import DocumentHandler.Document

  def apply(): DocumentHandler = new Impl

  private final class Impl extends DocumentHandler with ModelImpl[DocumentHandler.Update] {
    override def toString = "DocumentHandler"

    private val all   = STMRef(Vec.empty[Document])
    private val map   = TMap.empty[File, Document] // path to document

    // def openRead(path: String): Document = ...

    def addDocument[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      doc.folder.foreach { p =>
        require(!map.contains(p), s"Workspace for path '$p' is already registered")
        all.transform(_ :+ doc)
        map += p -> doc
      }

      doc.addDependent(new Disposable[S#Tx] {
        def dispose()(implicit tx: S#Tx): Unit = removeDoc(doc)
      })

      deferTx {
        dispatch(DocumentHandler.Opened(doc))
      }
    }

    private def deferTx(code: => Unit)(implicit tx: TxnLike): Unit = tx.afterCommit(code)

    private def removeDoc[S <: Sys[S]](doc: Workspace[S])(implicit tx: S#Tx): Unit = {
      implicit val itx = tx.peer
      all.transform { in =>
        val idx = in.indexOf(doc)
        require(idx >= 0, s"Workspace ${doc.folder.fold("") { p => s"for path '$p'" }} was not registered")
        in.patch(idx, Nil, 1)
      } (tx.peer)
      doc.folder.foreach(map -= _)

      deferTx {
        dispatch(DocumentHandler.Closed(doc))
      }
    }

    def allDocuments: Iterator[Document] = all.single().iterator
    def getDocument(file: File): Option[Document] = map.single.get(file)

    def isEmpty: Boolean = map.single.isEmpty
  }
}