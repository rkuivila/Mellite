package de.sciss
package mellite
package gui

import TreeTable.Path
import scala.swing.event.Event
import collection.immutable.{IndexedSeq => Vec}

final case class TreeTableSelectionChanged[A, Col <: TreeColumnModel[A]](
    source:               TreeTable[A, Col],
    pathsAdded:           Vec[Path[A]],
    pathsRemoved:         Vec[Path[A]],
    newLeadSelectionPath: Option[Path[A]],
    oldLeadSelectionPath: Option[Path[A]])
  extends Event // TreeEvent[A] // with SelectionEvent
