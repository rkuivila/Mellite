package de.sciss.mellite.gui

import scala.swing.event.Event

final case class TreeColumnChanged[A](model: TreeColumnModel[A], path: TreeTable.Path[A], column: Int) extends Event