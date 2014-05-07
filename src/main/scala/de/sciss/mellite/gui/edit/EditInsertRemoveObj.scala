package de.sciss.mellite.gui.edit

import de.sciss.lucre.stm
import de.sciss.lucre.event.Sys
import javax.swing.undo.{UndoableEdit, CannotRedoException, CannotUndoException, AbstractUndoableEdit}
import de.sciss.synth.proc.{Obj, Folder}

// direction: true = insert, false = remove
private[edit] class EditInsertRemoveObj[S <: Sys[S]](direction: Boolean,
                         parentH: stm.Source[S#Tx, Folder[S]],
                         index: Int,
                         childH: stm.Source[S#Tx, Obj[S]])(implicit cursor: stm.Cursor[S])
  extends AbstractUndoableEdit {

  override def undo(): Unit = {
    super.undo()
    cursor.step { implicit tx =>
      val success = if (direction) remove() else insert()
      if (!success) throw new CannotUndoException()
    }
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

  private def insert()(implicit tx: S#Tx): Boolean = {
    val parent = parentH()
    if (parent.size >= index) {
      val child = childH()
      parent.insert(index, child)
      true
    } else false
  }

  private def remove()(implicit tx: S#Tx): Boolean = {
    val parent = parentH()
    if (parent.size > index) {
      parent.removeAt(index)
      true
    } else false
  }

  def perform()(implicit tx: S#Tx): Unit = {
    val success = if (direction) insert() else remove()
    if (!success) throw new CannotRedoException()
  }
}

object EditInsertObj {
  def apply[S <: Sys[S]](nodeType: String, parentH: stm.Source[S#Tx, Folder[S]], index: Int,
                         childH: stm.Source[S#Tx, Obj[S]])(implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val res = new Impl(nodeType, parentH, index, childH)
    res.perform()
    res
  }

  private class Impl[S <: Sys[S]](nodeType: String,
                                  parentH: stm.Source[S#Tx, Folder[S]],
                                  index: Int,
                                  childH: stm.Source[S#Tx, Obj[S]])(implicit cursor: stm.Cursor[S])
    extends EditInsertRemoveObj[S](true, parentH, index, childH) {

    override def getPresentationName = s"Insert $nodeType"
  }
}

object EditRemoveObj {
  def apply[S <: Sys[S]](nodeType: String, parentH: stm.Source[S#Tx, Folder[S]], index: Int,
                         childH: stm.Source[S#Tx, Obj[S]])(implicit tx: S#Tx, cursor: stm.Cursor[S]): UndoableEdit = {
    val res = new Impl(nodeType, parentH, index, childH)
    res.perform()
    res
  }

  private class Impl[S <: Sys[S]](nodeType: String, parentH: stm.Source[S#Tx, Folder[S]], index: Int,
                                  childH: stm.Source[S#Tx, Obj[S]])(implicit cursor: stm.Cursor[S])
    extends EditInsertRemoveObj[S](false, parentH, index, childH) {

    override def getPresentationName = s"Remove $nodeType"
  }
}