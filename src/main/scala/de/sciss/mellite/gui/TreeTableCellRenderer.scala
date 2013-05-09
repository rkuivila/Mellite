package de.sciss
package mellite
package gui

import treetable.j
import scala.swing.{Label, Component}

object TreeTableCellRenderer {
  final case class State(selected: Boolean, focused: Boolean, expanded: Boolean, leaf: Boolean)

  object Default extends Label with Wrapped[Any] {
    override lazy val peer: j.DefaultTreeTableCellRenderer = new j.DefaultTreeTableCellRenderer

    def getRendererComponent(treeTable: TreeTable[_, _], value: Any, row: Int, column: Int, state: State): Component = {
      if (treeTable.hierarchicalColumn == column) {
        peer.getTreeTableCellRendererComponent(treeTable.peer, value, state.selected, state.focused, row, column,
          state.expanded, state.leaf)
      } else {
        peer.getTreeTableCellRendererComponent(treeTable.peer, value, state.selected, state.focused, row, column)
      }
      this
    }
  }

  trait Wrapped[-A] extends TreeTableCellRenderer[A] {
    def peer: j.TreeTableCellRenderer
  }
}
trait TreeTableCellRenderer[-A] {
  import TreeTableCellRenderer._

  def getRendererComponent(treeTable: TreeTable[_, _], value: A, row: Int, column: Int, state: State): Component
}