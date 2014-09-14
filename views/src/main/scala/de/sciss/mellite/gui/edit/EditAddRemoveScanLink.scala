/*
 *  EditAddRemoveScanLink.scala
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
package gui
package edit

import javax.swing.undo.{UndoableEdit, CannotRedoException, CannotUndoException, AbstractUndoableEdit}

import de.sciss.lucre.stm
import de.sciss.synth.proc.Scan
import de.sciss.lucre.event.Sys

// direction: true = insert, false = remove
private[edit] final class EditAddRemoveScanLink[S <: Sys[S]](isAdd: Boolean,
                                                       sourceH: stm.Source[S#Tx, Scan[S]],
                                                       sourceKey: String,
                                                       sinkH: stm.Source[S#Tx, Scan[S]],
                                                       sinkKey: String)(implicit cursor: stm.Cursor[S])
  extends AbstractUndoableEdit {

  override def undo(): Unit = {
    super.redo()
    cursor.step { implicit tx => perform(isUndo = true) }
  }

  override def redo(): Unit = {
    super.redo()
    cursor.step { implicit tx => perform() }
  }

  override def die(): Unit = {
    val hasBeenDone = canUndo
    super.die()
    if (!hasBeenDone) {
      // XXX TODO: dispose()
    }
  }

  def perform()(implicit tx: S#Tx): Unit = perform(isUndo = false)

  private def perform(isUndo: Boolean)(implicit tx: S#Tx): Unit = {
    val source  = sourceH()
    val sink    = sinkH  ()
    val link    = Scan.Link.Scan(sink)
    val success = if (isAdd ^ isUndo)
      source.addSink   (link)
    else
      source.removeSink(link)

    if (!success) {
      if (isUndo) throw new CannotUndoException()
      else        throw new CannotRedoException()
    }
  }

  override def getPresentationName = s"${if (isAdd) "Add" else "Remove"} Link"
}
object EditAddScanLink {
  def apply[S <: Sys[S]](source   : Scan[S],
                         sourceKey: String,
                         sink     : Scan[S],
                         sinkKey  : String)(implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val sourceH = tx.newHandle(source)
    val sinkH   = tx.newHandle(sink)
    val res = new EditAddRemoveScanLink(isAdd = true, sourceH = sourceH, sourceKey = sourceKey,
      sinkH = sinkH, sinkKey = sinkKey)
    res.perform()
    res
  }
}

object EditRemoveScanLink {
  def apply[S <: Sys[S]](source   : Scan[S],
                         sourceKey: String,
                         sink     : Scan[S],
                         sinkKey  : String)(implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val sourceH = tx.newHandle(source)
    val sinkH   = tx.newHandle(sink)
    val res = new EditAddRemoveScanLink(isAdd = false, sourceH = sourceH, sourceKey = sourceKey,
      sinkH = sinkH, sinkKey = sinkKey)
    res.perform()
    res
  }
}